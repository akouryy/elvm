#include <ir/ir.h>
#include <target/util.h>
#include <string.h>

static const char* HS_DEFAULT_REG_NAMES[6] = {
  "a", "b", "c", "d", "bp", "sp"
};

static int hs_reg_count[6];

static void hs_reset_reg() {
  for (int i = 0; i < 6; i++) {
    hs_reg_count[i] = 0;
    reg_names[i] = HS_DEFAULT_REG_NAMES[i];
  }
}

static void hs_inc_reg(int reg) {
  hs_reg_count[reg]++;
  reg_names[reg] = format("%s%d", HS_DEFAULT_REG_NAMES[reg], hs_reg_count[reg]);
}

static char* hs_reg_str() {
  return format("%s %s %s %s %s %s", reg_names[0], reg_names[1],
                reg_names[2], reg_names[3], reg_names[4], reg_names[5]);
}

static char* hs_cmp_op_str(Op op) {
  switch (normalize_cond(op, 0)) {
    case JEQ: return "==";
    case JNE: return "/=";
    case JLT: return "<";
    case JGT: return ">";
    case JLE: return "<=";
    case JGE: return ">=";
    default: error(format("oops! unknown operator %d", op));
  }
}

static void header_hs(Data* data) {
  puts(
    "import Data.Array.IO as A\n"
    "import Data.Bits ((.&.))\n"
    "import Data.Char (chr, ord)\n"
    "import System.IO (isEOF)\n"
    "import System.Exit (exitSuccess)\n"
    "\n"
    "type Memory = A.IOUArray Int Int\n"
    "\n"
    "add :: Int -> Int -> Int\n"
    "add x y = (x + y) .&. " UINT_MAX_STR "\n"
    "\n"
    "sub :: Int -> Int -> Int\n"
    "sub x y = (x - y) .&. " UINT_MAX_STR "\n"
    "\n"
    "putc :: Int -> IO ()\n"
    "putc c = putChar . chr $ c `mod` 256\n"
    "\n"
    "getc :: IO Int\n"
    "getc = do\n"
    " eof <- isEOF\n"
    " if eof\n"
    "  then return 0\n"
    "  else ord <$> getChar\n"
    "\n"
    "main :: IO ()\n"
    "main = do\n"
    " mem <- A.newListArray (0, " UINT_MAX_STR ") $ ["
  );
  inc_indent();
  inc_indent();

  for (int mp = 0; data; data = data->next, mp++) {
    if (mp % 10 == 0) {
      if (mp != 0)
        printf(",\n");
      emit_indent();
    } else  {
      printf(", ");
    }
    printf("%d", data->v);
  }
  putchar('\n');
  emit_line("] ++ [0, 0..]");
  dec_indent();

  emit_line("run 0 0 0 0 0 0 0 mem");
  dec_indent();
  emit_line("");
  emit_line("run :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> "
            "Memory -> IO ()");
}

static void hs_emit_func_prologue(int func_id) {
  (void)func_id; // unused
}

static void hs_emit_func_epilogue(void) {}

static bool hs_no_jump = false;

static void hs_emit_pc_change(int pc) {
  if (hs_no_jump)
    emit_line("run (succ pc) %s mem", hs_reg_str());
  if (pc > 0)
    dec_indent();
  hs_no_jump = true;
  hs_reset_reg();
  emit_line("");
  emit_line("run pc@%d %s mem = do", pc, hs_reg_str());
  inc_indent();
}

static void footer_hs() {
  if (hs_no_jump)
    emit_line("run (succ pc) %s mem", hs_reg_str());
  dec_indent();
  emit_line("");
  emit_line("run _ _ _ _ _ _ _ _ = return ()");
}

static void hs_emit_inst(Inst* inst) {
  const char *src, *dst;

  switch (inst->op) {
  case MOV:
    reg_names[inst->dst.reg] = src_str(inst);
    break;

  case ADD:
    src = src_str(inst);
    dst = reg_names[inst->dst.reg];
    hs_inc_reg(inst->dst.reg);
    emit_line("let %s = %s `add` %s", reg_names[inst->dst.reg], dst, src);
    break;

  case SUB:
    src = src_str(inst);
    dst = reg_names[inst->dst.reg];
    hs_inc_reg(inst->dst.reg);
    emit_line("let %s = %s `sub` %s", reg_names[inst->dst.reg], dst, src);
    break;

  case LOAD:
    src = src_str(inst);
    hs_inc_reg(inst->dst.reg);
    emit_line("%s <- A.readArray mem %s", reg_names[inst->dst.reg], src);
    break;

  case STORE:
    emit_line("A.writeArray mem %s %s", src_str(inst), reg_names[inst->dst.reg]);
    break;

  case PUTC:
    emit_line("putc %s", src_str(inst));
    break;

  case GETC:
    hs_inc_reg(inst->dst.reg);
    emit_line("%s <- getc", reg_names[inst->dst.reg]);
    break;

  case EXIT:
    emit_line("exitSuccess");
    break;

  case DUMP:
    break;

  case EQ:
  case NE:
  case LT:
  case GT:
  case LE:
  case GE:
    src = src_str(inst);
    dst = reg_names[inst->dst.reg];
    hs_inc_reg(inst->dst.reg);
    emit_line("let %s = fromEnum $ %s %s %s",
              reg_names[inst->dst.reg], dst, hs_cmp_op_str(inst->op), src);
    break;

  case JEQ:
  case JNE:
  case JLT:
  case JGT:
  case JLE:
  case JGE:
    hs_no_jump = false;
    emit_line("run (if %s %s %s then %s else succ pc) %s mem",
              reg_names[inst->dst.reg], hs_cmp_op_str(inst->op), src_str(inst),
              value_str(&inst->jmp), hs_reg_str());
    break;

  case JMP:
    hs_no_jump = false;
    emit_line("run %s %s mem", value_str(&inst->jmp), hs_reg_str());
    break;

  default:
    error(format("oops! unknown operation %d", inst->op));
  }
}

void target_hs(Module* module) {
  header_hs(module->data);

  emit_chunked_main_loop(module->text,
                         hs_emit_func_prologue,
                         hs_emit_func_epilogue,
                         hs_emit_pc_change,
                         hs_emit_inst);

  footer_hs();
}
