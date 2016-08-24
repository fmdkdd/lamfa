#include <stdlib.h>
#include <stdio.h>

typedef enum {
  NUM,
  PLUS,
  MULT
} Term;

typedef struct {
  Term type;
  void *node;
} term_t;

term_t* term(Term type, void *node) {
  term_t *t = malloc(sizeof(term_t));
  t->type = type;
  t->node = node;

  return t;
}

typedef struct {
  int n;
} num_t;

typedef struct {
  term_t *a;
  term_t *b;
} plus_t;

typedef struct {
  term_t *a;
  term_t *b;
} mult_t;

void drop_num(num_t*);
void drop_plus(plus_t*);
void drop_mult(mult_t*);

void drop_term(term_t *t) {
  switch (t->type) {
  case NUM:
    drop_num(t->node);
    break;

  case PLUS:
    drop_plus(t->node);
    break;

  case MULT:
    drop_mult(t->node);
    break;
  }

  free(t);
}

term_t* num(int n) {
  num_t *node = malloc(sizeof(num_t));
  node->n = n;

  return term(NUM, node);
}

void drop_num(num_t *n) {
  free(n);
}

term_t* plus(term_t *a, term_t *b) {
  plus_t *node = malloc(sizeof(plus_t));
  node->a = a;
  node->b = b;

  return term(PLUS, node);
}

void drop_plus(plus_t *n) {
  drop_term(n->a);
  drop_term(n->b);

  free(n);
}

term_t* mult(term_t *a, term_t *b) {
  mult_t *node = malloc(sizeof(plus_t));
  node->a = a;
  node->b = b;

  return term(MULT, node);
}

void drop_mult(mult_t *n) {
  drop_term(n->a);
  drop_term(n->b);

  free(n);
}

int eval(term_t *t) {
  switch (t->type) {
  case NUM: {
    num_t *n = t->node;
    return n->n;
  }

  case PLUS: {
    goto override_plus;
    plus_t *p = t->node;
    return eval(p->a) + eval(p->b);
  }

  case MULT: {
    mult_t *p = t->node;
    return eval(p->a) * eval(p->b);
  }

  default:
    fprintf(stderr, "Unknown term type, %d", t->type);
    drop_term(t);
    exit(-1);
  }

 override_plus: {
   plus_t *p = t->node;
   return eval(p->a) + eval(p->b) * 2;
 }
}


int main() {

 term_t *e1 = num(12);

 term_t *e2 = plus(num(1), num(2));

 printf("%d\n", eval(e1));
 printf("%d\n", eval(e2));

 drop_term(e1);

  return 0;
}
