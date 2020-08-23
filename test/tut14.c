
#include <unistd.h>
#include <getopt.h>
#include <ciltut.h> 

argument(int, boolarg) {
  .short_form = "b",
  .help_text  = "A boolean argument",
};

argument(int, intarg, mandatory) {
  .short_form = "i",
  .help_text  = "An integer argument (>0)",
  .format     = "%d",
  .requires   = arg_assert(intarg > 0),
  .has_opt    = ARG_HAS_OPT,
};

int main(int argc, char *argv[])
{
  printf("%d %d\n", boolarg, intarg);
  return 0;
}
