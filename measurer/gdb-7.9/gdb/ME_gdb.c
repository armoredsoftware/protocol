//#include "printcmd.c"

#include "ME_gdb.h"

/*char * BE_get_variable (const char *exp, int voidprint)
{
  printf("Entered: BE_get_variable\n");

  char * result;
  result = NULL;

  struct expression *expr = NULL;
  struct cleanup *old_chain = make_cleanup (null_cleanup, NULL);
  char format = 0;
  struct value *val;
  struct format_data fmt;

  if (exp && *exp == '/')
    {
      exp++;
      fmt = decode_format (&exp, last_format, 0);
      validate_format (fmt, "print");
      last_format = format = fmt.format;
    }
  else
    {
      fmt.count = 1;
      fmt.format = 0;
      fmt.size = 0;
      fmt.raw = 0;
    }

  if (exp && *exp)
    {
      expr = parse_expression (exp);
      if (!expr) {
	result = (char*)malloc(sizeof(char) * 64); //max length?...
	strcpy(result, "Could not parse!");
	return result;

      }
      make_cleanup (free_current_contents, &expr);
      val = evaluate_expression (expr);
    }
  else
    val = access_value_history (0);
  if (voidprint || (val && value_type (val) &&
		    TYPE_CODE (value_type (val)) != TYPE_CODE_VOID))
    {

      struct value_print_options opts;
      //int histindex = record_latest_value (val);
      get_formatted_print_options (&opts, format);
      opts.raw = fmt.raw;


      result = (char*)malloc(sizeof(char) * 64); //max length?...
      FILE *stream;
      stream = fmemopen (result, strlen (result), "w");
      struct ui_file *temp = stdio_fileopen(stream);
      print_formatted (val, fmt.size, &opts, temp);
      fclose(stream);
      free(temp);//free ui_file or something???

      printf("Value of result = \"%s\"\n", result);

    }

  do_cleanups (old_chain);

  printf("Exited: BE_get_variable\n");

  return result;
  }*/
