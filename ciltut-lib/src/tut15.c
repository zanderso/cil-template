#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/alloc.h>

static int ocaml_initialized = 0;

static void init_ocaml()
{
	if (!ocaml_initialized) {
		char *argv[2] = {"program",NULL};
		caml_startup(argv);
		ocaml_initialized = 1;	
	}
	return;
}

void assign(uint64_t lhs, uint64_t op, int opk, uint64_t opv)
{
	static value *assign_closure = NULL;
	value args[4];
	init_ocaml();
	if (assign_closure == NULL) {
		assign_closure = caml_named_value("assign");
	}

	args[0] = caml_copy_int64(lhs);
	args[1] = caml_copy_int64(op);
	args[2] = Val_int(opk);
	args[3] = caml_copy_int64(opv);
	caml_callbackN(*assign_closure, 4, args);
	return;
}

void assgn_bop(uint64_t lhs, uint64_t lhsv, int bop,
               uint64_t op1, int op1k, uint64_t op1v,
               uint64_t op2, int op2k, uint64_t op2v)
{
	static value *assgn_bop_closure = NULL;
	value args[9];
	init_ocaml();
	if (assgn_bop_closure == NULL)  {
		assgn_bop_closure = caml_named_value("assgn_bop");
	}

	args[0] = caml_copy_int64(lhs);
	args[1] = caml_copy_int64(lhsv);
	args[2] = Val_int(bop);
	args[3] = caml_copy_int64(op1);
	args[4] = Val_int(op1k);
	args[5] = caml_copy_int64(op1v);
	args[6] = caml_copy_int64(op2);
	args[7] = Val_int(op2k);
	args[8] = caml_copy_int64(op2v);
	caml_callbackN(*assgn_bop_closure, 9, args);
	return;
}

void assgn_uop(uint64_t lhs, uint64_t lhsv, int uop,
               uint64_t op, int opk, uint64_t opv)
{
	static value *assgn_uop_closure = NULL;
	value args[6];
	init_ocaml();
	if (assgn_uop_closure == NULL)  {
		assgn_uop_closure = caml_named_value("assgn_uop");
	}

	args[0] = caml_copy_int64(lhs);
	args[1] = caml_copy_int64(lhsv);
	args[2] = Val_int(uop);
	args[3] = caml_copy_int64(op);
	args[4] = Val_int(opk);
	args[5] = caml_copy_int64(opv);
	caml_callbackN(*assgn_uop_closure, 6, args);
	return;
}

void cond(int cid, int r, uint64_t op, int opk, uint64_t opv)
{
	static value *cond_closure = NULL;
	value args[5];
	init_ocaml();
	if (cond_closure == NULL)  {
		cond_closure = caml_named_value("cond");
	}

	args[0] = Val_int(cid);
	args[1] = Val_int(r);
	args[2] = caml_copy_int64(op);
	args[3] = Val_int(opk);
	args[4] = caml_copy_int64(opv);
	caml_callbackN(*cond_closure, 5, args);
	return;
}

void cond_bop(int cid, int bop, int r,
              uint64_t op1, int op1k, uint64_t op1v,
              uint64_t op2, int op2k, uint64_t op2v)
{
	static value *cond_bop_closure = NULL;
	value args[9];
	init_ocaml();
	if (cond_bop_closure == NULL)  {
		cond_bop_closure = caml_named_value("cond_bop");
	}

	args[0] = Val_int(cid);
	args[1] = Val_int(bop);
	args[2] = Val_int(r);
	args[3] = caml_copy_int64(op1);
	args[4] = Val_int(op1k);
	args[5] = caml_copy_int64(op1v);
	args[6] = caml_copy_int64(op2);
	args[7] = Val_int(op2k);
	args[8] = caml_copy_int64(op2v);
	caml_callbackN(*cond_bop_closure, 9, args);
	return;
}

void cond_uop(int cid, int uop, int r,
              uint64_t op, int opk, uint64_t opv)
{
	static value *cond_uop_closure = NULL;
	value args[6];
	init_ocaml();
	if (cond_uop_closure == NULL)  {
		cond_uop_closure = caml_named_value("cond_uop");
	}

	args[0] = Val_int(cid);
	args[1] = Val_int(uop);
	args[2] = Val_int(r);
	args[3] = caml_copy_int64(op);
	args[4] = Val_int(opk);
	args[5] = caml_copy_int64(opv);
	caml_callbackN(*cond_uop_closure, 6, args);
	return;
}

void register_input(char *name, uint64_t addr, int bits)
{
	static value *register_input_closure = NULL;
	init_ocaml();
	if (register_input_closure == NULL) {
		register_input_closure = caml_named_value("register_input");
	}

	caml_callback3(*register_input_closure, caml_copy_string(name),
	                                        caml_copy_int64(addr),
	                                        Val_long(bits));
	return;
}

void register_arr_input(char *name, uint64_t start, int sz, int cnt)
{
	int namelen = strlen(name);
	char buf[namelen + 10];
	int i;

	for (i = 0; i < cnt; i++) {
		snprintf(buf, namelen + 10, "%s%d", name, i);
		register_input(buf, start + i * sz, sz * 8);
	}

	return;
}

void register_nt_input(char *name, char *start)
{
	int namelen = strlen(name);
	char buf[namelen + 10];
	int i = 0;

	char *ptr = start;
	do {
		snprintf(buf, namelen + 10, "%s%d", name, i);
		register_input(buf, (uint64_t)ptr, 8);
		i++;
	} while (*ptr++);

	return;
}

int autotest_finished = 0;

void gen_new_input()
{
	static value *gen_new_input_closure = NULL;
	init_ocaml();
	if (gen_new_input_closure == NULL) {
		gen_new_input_closure = caml_named_value("gen_new_input");
	}
	value v = caml_callback(*gen_new_input_closure, Val_unit);
	if (Int_val(v) == 0) {
		autotest_finished = 1;
	}
	return;
}

void push_val(uint64_t v)
{
	static value *push_val_closure = NULL;
	init_ocaml();
	if (push_val_closure == NULL) {
		push_val_closure = caml_named_value("push_val");
	}

	caml_callback(*push_val_closure, caml_copy_int64(v));
	return;
}

static uint64_t pop_val_opt_print(int print, char *name)
{
	static value *pop_val_closure = NULL;
	init_ocaml();
	if (pop_val_closure == NULL) {
		pop_val_closure = caml_named_value("pop_val");
	}

	value r = caml_callback(*pop_val_closure, caml_copy_string(name));
	if (print) {
		printf("%s <- %llu\n", name, (unsigned long long)Int64_val(r));
	}
	return Int64_val(r);
}

uint64_t pop_val(char *name)
{
	return pop_val_opt_print(1, name);
}

void pop_array(char *name, char *base, int cnt, int sz)
{
	int namelen = strlen(name);
	char buf[namelen + 10];
	int i;

	for (i = 0; i < cnt; i++) {
		snprintf(buf, namelen + 10, "%s%d", name, i);
		switch (sz) {
			case 1:
				*(base + i) = (uint8_t)pop_val(buf);
				break;
			case 2:
				*(uint16_t *)(base + i*sz) = (uint16_t)pop_val(buf);
			case 4:
				*(uint32_t *)(base + i*sz) = (uint32_t)pop_val(buf);
			default:
				*(uint64_t *)(base + i*sz) = pop_val(buf);
		}
	}

	return;
}

void pop_nt(char *name, char *base)
{
	int namelen = strlen(name);
	char buf[namelen + 10];
	int i = 0;

	char *ptr = base;
	char c;
	do {
		snprintf(buf, namelen + 10, "%s%d", name, i);
		c = (char)pop_val_opt_print(0, buf);
		*ptr = c; ptr++; i++;
	} while(c);

	printf ("%s <- %s\n", name, base);

	return;
}

void return_push(uint64_t p, uint64_t v)
{
	static value *return_push_closure = NULL;
	init_ocaml();
	if (return_push_closure == NULL) {
		return_push_closure = caml_named_value("return_push");
	}

	caml_callback2(*return_push_closure, caml_copy_int64(p), caml_copy_int64(v));
	return;
}

void return_pop(uint64_t p, uint64_t v)
{
	static value *return_pop_closure = NULL;
	init_ocaml();
	if (return_pop_closure == NULL) {
		return_pop_closure = caml_named_value("return_pop");
	}

	caml_callback2(*return_pop_closure, caml_copy_int64(p), caml_copy_int64(v));
	return;
}

void autotest_reset()
{
	static value *autotest_reset_closure = NULL;
	init_ocaml();
	if (autotest_reset_closure == NULL) {
		autotest_reset_closure = caml_named_value("autotest_reset");
	}
	caml_callback(*autotest_reset_closure , Val_unit);
	autotest_finished = 0;
	return;
}
