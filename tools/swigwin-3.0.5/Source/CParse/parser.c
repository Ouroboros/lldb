/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 22 "parser.y" /* yacc.c:339  */

#define yylex yylex

#include "swig.h"
#include "cparse.h"
#include "preprocessor.h"
#include <ctype.h>

/* We do this for portability */
#undef alloca
#define alloca malloc

/* -----------------------------------------------------------------------------
 *                               Externals
 * ----------------------------------------------------------------------------- */

int  yyparse();

/* NEW Variables */

static Node    *top = 0;      /* Top of the generated parse tree */
static int      unnamed = 0;  /* Unnamed datatype counter */
static Hash    *classes = 0;        /* Hash table of classes */
static Hash    *classes_typedefs = 0; /* Hash table of typedef classes: typedef struct X {...} Y; */
static Symtab  *prev_symtab = 0;
static Node    *current_class = 0;
String  *ModuleName = 0;
static Node    *module_node = 0;
static String  *Classprefix = 0;  
static String  *Namespaceprefix = 0;
static int      inclass = 0;
static Node    *currentOuterClass = 0; /* for nested classes */
static char    *last_cpptype = 0;
static int      inherit_list = 0;
static Parm    *template_parameters = 0;
static int      extendmode   = 0;
static int      compact_default_args = 0;
static int      template_reduce = 0;
static int      cparse_externc = 0;
int		ignore_nested_classes = 0;
int		kwargs_supported = 0;
/* -----------------------------------------------------------------------------
 *                            Assist Functions
 * ----------------------------------------------------------------------------- */


 
/* Called by the parser (yyparse) when an error is found.*/
static void yyerror (const char *e) {
  (void)e;
}

/* Copies a node.  Does not copy tree links or symbol table data (except for
   sym:name) */

static Node *copy_node(Node *n) {
  Node *nn;
  Iterator k;
  nn = NewHash();
  Setfile(nn,Getfile(n));
  Setline(nn,Getline(n));
  for (k = First(n); k.key; k = Next(k)) {
    String *ci;
    String *key = k.key;
    char *ckey = Char(key);
    if ((strcmp(ckey,"nextSibling") == 0) ||
	(strcmp(ckey,"previousSibling") == 0) ||
	(strcmp(ckey,"parentNode") == 0) ||
	(strcmp(ckey,"lastChild") == 0)) {
      continue;
    }
    if (Strncmp(key,"csym:",5) == 0) continue;
    /* We do copy sym:name.  For templates */
    if ((strcmp(ckey,"sym:name") == 0) || 
	(strcmp(ckey,"sym:weak") == 0) ||
	(strcmp(ckey,"sym:typename") == 0)) {
      String *ci = Copy(k.item);
      Setattr(nn,key, ci);
      Delete(ci);
      continue;
    }
    if (strcmp(ckey,"sym:symtab") == 0) {
      Setattr(nn,"sym:needs_symtab", "1");
    }
    /* We don't copy any other symbol table attributes */
    if (strncmp(ckey,"sym:",4) == 0) {
      continue;
    }
    /* If children.  We copy them recursively using this function */
    if (strcmp(ckey,"firstChild") == 0) {
      /* Copy children */
      Node *cn = k.item;
      while (cn) {
	Node *copy = copy_node(cn);
	appendChild(nn,copy);
	Delete(copy);
	cn = nextSibling(cn);
      }
      continue;
    }
    /* We don't copy the symbol table.  But we drop an attribute 
       requires_symtab so that functions know it needs to be built */

    if (strcmp(ckey,"symtab") == 0) {
      /* Node defined a symbol table. */
      Setattr(nn,"requires_symtab","1");
      continue;
    }
    /* Can't copy nodes */
    if (strcmp(ckey,"node") == 0) {
      continue;
    }
    if ((strcmp(ckey,"parms") == 0) || (strcmp(ckey,"pattern") == 0) || (strcmp(ckey,"throws") == 0)
	|| (strcmp(ckey,"kwargs") == 0)) {
      ParmList *pl = CopyParmList(k.item);
      Setattr(nn,key,pl);
      Delete(pl);
      continue;
    }
    if (strcmp(ckey,"nested:outer") == 0) { /* don't copy outer classes links, they will be updated later */
      Setattr(nn, key, k.item);
      continue;
    }
    /* Looks okay.  Just copy the data using Copy */
    ci = Copy(k.item);
    Setattr(nn, key, ci);
    Delete(ci);
  }
  return nn;
}

/* -----------------------------------------------------------------------------
 *                              Variables
 * ----------------------------------------------------------------------------- */

static char  *typemap_lang = 0;    /* Current language setting */

static int cplus_mode  = 0;

/* C++ modes */

#define  CPLUS_PUBLIC    1
#define  CPLUS_PRIVATE   2
#define  CPLUS_PROTECTED 3

/* include types */
static int   import_mode = 0;

void SWIG_typemap_lang(const char *tm_lang) {
  typemap_lang = Swig_copy_string(tm_lang);
}

void SWIG_cparse_set_compact_default_args(int defargs) {
  compact_default_args = defargs;
}

int SWIG_cparse_template_reduce(int treduce) {
  template_reduce = treduce;
  return treduce;  
}

/* -----------------------------------------------------------------------------
 *                           Assist functions
 * ----------------------------------------------------------------------------- */

static int promote_type(int t) {
  if (t <= T_UCHAR || t == T_CHAR) return T_INT;
  return t;
}

/* Perform type-promotion for binary operators */
static int promote(int t1, int t2) {
  t1 = promote_type(t1);
  t2 = promote_type(t2);
  return t1 > t2 ? t1 : t2;
}

static String *yyrename = 0;

/* Forward renaming operator */

static String *resolve_create_node_scope(String *cname);


Hash *Swig_cparse_features(void) {
  static Hash   *features_hash = 0;
  if (!features_hash) features_hash = NewHash();
  return features_hash;
}

/* Fully qualify any template parameters */
static String *feature_identifier_fix(String *s) {
  String *tp = SwigType_istemplate_templateprefix(s);
  if (tp) {
    String *ts, *ta, *tq;
    ts = SwigType_templatesuffix(s);
    ta = SwigType_templateargs(s);
    tq = Swig_symbol_type_qualify(ta,0);
    Append(tp,tq);
    Append(tp,ts);
    Delete(ts);
    Delete(ta);
    Delete(tq);
    return tp;
  } else {
    return NewString(s);
  }
}

static void set_access_mode(Node *n) {
  if (cplus_mode == CPLUS_PUBLIC)
    Setattr(n, "access", "public");
  else if (cplus_mode == CPLUS_PROTECTED)
    Setattr(n, "access", "protected");
  else
    Setattr(n, "access", "private");
}

static void restore_access_mode(Node *n) {
  String *mode = Getattr(n, "access");
  if (Strcmp(mode, "private") == 0)
    cplus_mode = CPLUS_PRIVATE;
  else if (Strcmp(mode, "protected") == 0)
    cplus_mode = CPLUS_PROTECTED;
  else
    cplus_mode = CPLUS_PUBLIC;
}

/* Generate the symbol table name for an object */
/* This is a bit of a mess. Need to clean up */
static String *add_oldname = 0;



static String *make_name(Node *n, String *name,SwigType *decl) {
  int destructor = name && (*(Char(name)) == '~');

  if (yyrename) {
    String *s = NewString(yyrename);
    Delete(yyrename);
    yyrename = 0;
    if (destructor  && (*(Char(s)) != '~')) {
      Insert(s,0,"~");
    }
    return s;
  }

  if (!name) return 0;
  return Swig_name_make(n,Namespaceprefix,name,decl,add_oldname);
}

/* Generate an unnamed identifier */
static String *make_unnamed() {
  unnamed++;
  return NewStringf("$unnamed%d$",unnamed);
}

/* Return if the node is a friend declaration */
static int is_friend(Node *n) {
  return Cmp(Getattr(n,"storage"),"friend") == 0;
}

static int is_operator(String *name) {
  return Strncmp(name,"operator ", 9) == 0;
}


/* Add declaration list to symbol table */
static int  add_only_one = 0;

static void add_symbols(Node *n) {
  String *decl;
  String *wrn = 0;

  if (inclass && n) {
    cparse_normalize_void(n);
  }
  while (n) {
    String *symname = 0;
    /* for friends, we need to pop the scope once */
    String *old_prefix = 0;
    Symtab *old_scope = 0;
    int isfriend = inclass && is_friend(n);
    int iscdecl = Cmp(nodeType(n),"cdecl") == 0;
    int only_csymbol = 0;
    
    if (inclass) {
      String *name = Getattr(n, "name");
      if (isfriend) {
	/* for friends, we need to add the scopename if needed */
	String *prefix = name ? Swig_scopename_prefix(name) : 0;
	old_prefix = Namespaceprefix;
	old_scope = Swig_symbol_popscope();
	Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	if (!prefix) {
	  if (name && !is_operator(name) && Namespaceprefix) {
	    String *nname = NewStringf("%s::%s", Namespaceprefix, name);
	    Setattr(n,"name",nname);
	    Delete(nname);
	  }
	} else {
	  Symtab *st = Swig_symbol_getscope(prefix);
	  String *ns = st ? Getattr(st,"name") : prefix;
	  String *base  = Swig_scopename_last(name);
	  String *nname = NewStringf("%s::%s", ns, base);
	  Setattr(n,"name",nname);
	  Delete(nname);
	  Delete(base);
	  Delete(prefix);
	}
	Namespaceprefix = 0;
      } else {
	/* for member functions, we need to remove the redundant
	   class scope if provided, as in
	   
	   struct Foo {
	   int Foo::method(int a);
	   };
	   
	*/
	String *prefix = name ? Swig_scopename_prefix(name) : 0;
	if (prefix) {
	  if (Classprefix && (Equal(prefix,Classprefix))) {
	    String *base = Swig_scopename_last(name);
	    Setattr(n,"name",base);
	    Delete(base);
	  }
	  Delete(prefix);
	}
      }
    }

    if (!isfriend && (inclass || extendmode)) {
      Setattr(n,"ismember","1");
    }

    if (extendmode) {
      Setattr(n,"isextendmember","1");
    }

    if (!isfriend && inclass) {
      if ((cplus_mode != CPLUS_PUBLIC)) {
	only_csymbol = 1;
	if (cplus_mode == CPLUS_PROTECTED) {
	  Setattr(n,"access", "protected");
	  only_csymbol = !Swig_need_protected(n);
	} else {
	  Setattr(n,"access", "private");
	  /* private are needed only when they are pure virtuals - why? */
	  if ((Cmp(Getattr(n,"storage"),"virtual") == 0) && (Cmp(Getattr(n,"value"),"0") == 0)) {
	    only_csymbol = 0;
	  }
	}
      } else {
	  Setattr(n,"access", "public");
      }
    }
    if (Getattr(n,"sym:name")) {
      n = nextSibling(n);
      continue;
    }
    decl = Getattr(n,"decl");
    if (!SwigType_isfunction(decl)) {
      String *name = Getattr(n,"name");
      String *makename = Getattr(n,"parser:makename");
      if (iscdecl) {	
	String *storage = Getattr(n, "storage");
	if (Cmp(storage,"typedef") == 0) {
	  Setattr(n,"kind","typedef");
	} else {
	  SwigType *type = Getattr(n,"type");
	  String *value = Getattr(n,"value");
	  Setattr(n,"kind","variable");
	  if (value && Len(value)) {
	    Setattr(n,"hasvalue","1");
	  }
	  if (type) {
	    SwigType *ty;
	    SwigType *tmp = 0;
	    if (decl) {
	      ty = tmp = Copy(type);
	      SwigType_push(ty,decl);
	    } else {
	      ty = type;
	    }
	    if (!SwigType_ismutable(ty) || (storage && Strstr(storage, "constexpr"))) {
	      SetFlag(n,"hasconsttype");
	      SetFlag(n,"feature:immutable");
	    }
	    if (tmp) Delete(tmp);
	  }
	  if (!type) {
	    Printf(stderr,"notype name %s\n", name);
	  }
	}
      }
      Swig_features_get(Swig_cparse_features(), Namespaceprefix, name, 0, n);
      if (makename) {
	symname = make_name(n, makename,0);
        Delattr(n,"parser:makename"); /* temporary information, don't leave it hanging around */
      } else {
        makename = name;
	symname = make_name(n, makename,0);
      }
      
      if (!symname) {
	symname = Copy(Getattr(n,"unnamed"));
      }
      if (symname) {
	wrn = Swig_name_warning(n, Namespaceprefix, symname,0);
      }
    } else {
      String *name = Getattr(n,"name");
      SwigType *fdecl = Copy(decl);
      SwigType *fun = SwigType_pop_function(fdecl);
      if (iscdecl) {	
	Setattr(n,"kind","function");
      }
      
      Swig_features_get(Swig_cparse_features(),Namespaceprefix,name,fun,n);

      symname = make_name(n, name,fun);
      wrn = Swig_name_warning(n, Namespaceprefix,symname,fun);
      
      Delete(fdecl);
      Delete(fun);
      
    }
    if (!symname) {
      n = nextSibling(n);
      continue;
    }
    if (cparse_cplusplus) {
      String *value = Getattr(n, "value");
      if (value && Strcmp(value, "delete") == 0) {
	/* C++11 deleted definition / deleted function */
        SetFlag(n,"deleted");
        SetFlag(n,"feature:ignore");
      }
    }
    if (only_csymbol || GetFlag(n,"feature:ignore")) {
      /* Only add to C symbol table and continue */
      Swig_symbol_add(0, n);
    } else if (strncmp(Char(symname),"$ignore",7) == 0) {
      char *c = Char(symname)+7;
      SetFlag(n,"feature:ignore");
      if (strlen(c)) {
	SWIG_WARN_NODE_BEGIN(n);
	Swig_warning(0,Getfile(n), Getline(n), "%s\n",c+1);
	SWIG_WARN_NODE_END(n);
      }
      Swig_symbol_add(0, n);
    } else {
      Node *c;
      if ((wrn) && (Len(wrn))) {
	String *metaname = symname;
	if (!Getmeta(metaname,"already_warned")) {
	  SWIG_WARN_NODE_BEGIN(n);
	  Swig_warning(0,Getfile(n),Getline(n), "%s\n", wrn);
	  SWIG_WARN_NODE_END(n);
	  Setmeta(metaname,"already_warned","1");
	}
      }
      c = Swig_symbol_add(symname,n);

      if (c != n) {
        /* symbol conflict attempting to add in the new symbol */
        if (Getattr(n,"sym:weak")) {
          Setattr(n,"sym:name",symname);
        } else {
          String *e = NewStringEmpty();
          String *en = NewStringEmpty();
          String *ec = NewStringEmpty();
          int redefined = Swig_need_redefined_warn(n,c,inclass);
          if (redefined) {
            Printf(en,"Identifier '%s' redefined (ignored)",symname);
            Printf(ec,"previous definition of '%s'",symname);
          } else {
            Printf(en,"Redundant redeclaration of '%s'",symname);
            Printf(ec,"previous declaration of '%s'",symname);
          }
          if (Cmp(symname,Getattr(n,"name"))) {
            Printf(en," (Renamed from '%s')", SwigType_namestr(Getattr(n,"name")));
          }
          Printf(en,",");
          if (Cmp(symname,Getattr(c,"name"))) {
            Printf(ec," (Renamed from '%s')", SwigType_namestr(Getattr(c,"name")));
          }
          Printf(ec,".");
	  SWIG_WARN_NODE_BEGIN(n);
          if (redefined) {
            Swig_warning(WARN_PARSE_REDEFINED,Getfile(n),Getline(n),"%s\n",en);
            Swig_warning(WARN_PARSE_REDEFINED,Getfile(c),Getline(c),"%s\n",ec);
          } else if (!is_friend(n) && !is_friend(c)) {
            Swig_warning(WARN_PARSE_REDUNDANT,Getfile(n),Getline(n),"%s\n",en);
            Swig_warning(WARN_PARSE_REDUNDANT,Getfile(c),Getline(c),"%s\n",ec);
          }
	  SWIG_WARN_NODE_END(n);
          Printf(e,"%s:%d:%s\n%s:%d:%s\n",Getfile(n),Getline(n),en,
                 Getfile(c),Getline(c),ec);
          Setattr(n,"error",e);
	  Delete(e);
          Delete(en);
          Delete(ec);
        }
      }
    }
    /* restore the class scope if needed */
    if (isfriend) {
      Swig_symbol_setscope(old_scope);
      if (old_prefix) {
	Delete(Namespaceprefix);
	Namespaceprefix = old_prefix;
      }
    }
    Delete(symname);

    if (add_only_one) return;
    n = nextSibling(n);
  }
}


/* add symbols a parse tree node copy */

static void add_symbols_copy(Node *n) {
  String *name;
  int    emode = 0;
  while (n) {
    char *cnodeType = Char(nodeType(n));

    if (strcmp(cnodeType,"access") == 0) {
      String *kind = Getattr(n,"kind");
      if (Strcmp(kind,"public") == 0) {
	cplus_mode = CPLUS_PUBLIC;
      } else if (Strcmp(kind,"private") == 0) {
	cplus_mode = CPLUS_PRIVATE;
      } else if (Strcmp(kind,"protected") == 0) {
	cplus_mode = CPLUS_PROTECTED;
      }
      n = nextSibling(n);
      continue;
    }

    add_oldname = Getattr(n,"sym:name");
    if ((add_oldname) || (Getattr(n,"sym:needs_symtab"))) {
      int old_inclass = -1;
      Node *old_current_class = 0;
      if (add_oldname) {
	DohIncref(add_oldname);
	/*  Disable this, it prevents %rename to work with templates */
	/* If already renamed, we used that name  */
	/*
	if (Strcmp(add_oldname, Getattr(n,"name")) != 0) {
	  Delete(yyrename);
	  yyrename = Copy(add_oldname);
	}
	*/
      }
      Delattr(n,"sym:needs_symtab");
      Delattr(n,"sym:name");

      add_only_one = 1;
      add_symbols(n);

      if (Getattr(n,"partialargs")) {
	Swig_symbol_cadd(Getattr(n,"partialargs"),n);
      }
      add_only_one = 0;
      name = Getattr(n,"name");
      if (Getattr(n,"requires_symtab")) {
	Swig_symbol_newscope();
	Swig_symbol_setscopename(name);
	Delete(Namespaceprefix);
	Namespaceprefix = Swig_symbol_qualifiedscopename(0);
      }
      if (strcmp(cnodeType,"class") == 0) {
	old_inclass = inclass;
	inclass = 1;
	old_current_class = current_class;
	current_class = n;
	if (Strcmp(Getattr(n,"kind"),"class") == 0) {
	  cplus_mode = CPLUS_PRIVATE;
	} else {
	  cplus_mode = CPLUS_PUBLIC;
	}
      }
      if (strcmp(cnodeType,"extend") == 0) {
	emode = cplus_mode;
	cplus_mode = CPLUS_PUBLIC;
      }
      add_symbols_copy(firstChild(n));
      if (strcmp(cnodeType,"extend") == 0) {
	cplus_mode = emode;
      }
      if (Getattr(n,"requires_symtab")) {
	Setattr(n,"symtab", Swig_symbol_popscope());
	Delattr(n,"requires_symtab");
	Delete(Namespaceprefix);
	Namespaceprefix = Swig_symbol_qualifiedscopename(0);
      }
      if (add_oldname) {
	Delete(add_oldname);
	add_oldname = 0;
      }
      if (strcmp(cnodeType,"class") == 0) {
	inclass = old_inclass;
	current_class = old_current_class;
      }
    } else {
      if (strcmp(cnodeType,"extend") == 0) {
	emode = cplus_mode;
	cplus_mode = CPLUS_PUBLIC;
      }
      add_symbols_copy(firstChild(n));
      if (strcmp(cnodeType,"extend") == 0) {
	cplus_mode = emode;
      }
    }
    n = nextSibling(n);
  }
}

/* Check a set of declarations to see if any are pure-abstract */

static List *pure_abstracts(Node *n) {
  List *abstracts = 0;
  while (n) {
    if (Cmp(nodeType(n),"cdecl") == 0) {
      String *decl = Getattr(n,"decl");
      if (SwigType_isfunction(decl)) {
	String *init = Getattr(n,"value");
	if (Cmp(init,"0") == 0) {
	  if (!abstracts) {
	    abstracts = NewList();
	  }
	  Append(abstracts,n);
	  SetFlag(n,"abstract");
	}
      }
    } else if (Cmp(nodeType(n),"destructor") == 0) {
      if (Cmp(Getattr(n,"value"),"0") == 0) {
	if (!abstracts) {
	  abstracts = NewList();
	}
	Append(abstracts,n);
	SetFlag(n,"abstract");
      }
    }
    n = nextSibling(n);
  }
  return abstracts;
}

/* Make a classname */

static String *make_class_name(String *name) {
  String *nname = 0;
  String *prefix;
  if (Namespaceprefix) {
    nname= NewStringf("%s::%s", Namespaceprefix, name);
  } else {
    nname = NewString(name);
  }
  prefix = SwigType_istemplate_templateprefix(nname);
  if (prefix) {
    String *args, *qargs;
    args   = SwigType_templateargs(nname);
    qargs  = Swig_symbol_type_qualify(args,0);
    Append(prefix,qargs);
    Delete(nname);
    Delete(args);
    Delete(qargs);
    nname = prefix;
  }
  return nname;
}

/* Use typedef name as class name */

static void add_typedef_name(Node *n, Node *decl, String *oldName, Symtab *cscope, String *scpname) {
  String *class_rename = 0;
  SwigType *decltype = Getattr(decl, "decl");
  if (!decltype || !Len(decltype)) {
    String *cname;
    String *tdscopename;
    String *class_scope = Swig_symbol_qualifiedscopename(cscope);
    String *name = Getattr(decl, "name");
    cname = Copy(name);
    Setattr(n, "tdname", cname);
    tdscopename = class_scope ? NewStringf("%s::%s", class_scope, name) : Copy(name);
    class_rename = Getattr(n, "class_rename");
    if (class_rename && (Strcmp(class_rename, oldName) == 0))
      Setattr(n, "class_rename", NewString(name));
    if (!classes_typedefs) classes_typedefs = NewHash();
    if (!Equal(scpname, tdscopename) && !Getattr(classes_typedefs, tdscopename)) {
      Setattr(classes_typedefs, tdscopename, n);
    }
    Setattr(n, "decl", decltype);
    Delete(class_scope);
    Delete(cname);
    Delete(tdscopename);
  }
}

/* If the class name is qualified.  We need to create or lookup namespace entries */

static Symtab *set_scope_to_global() {
  Symtab *symtab = Swig_symbol_global_scope();
  Swig_symbol_setscope(symtab);
  return symtab;
}
 
/* Remove the block braces, { and }, if the 'noblock' attribute is set.
 * Node *kw can be either a Hash or Parmlist. */
static String *remove_block(Node *kw, const String *inputcode) {
  String *modified_code = 0;
  while (kw) {
   String *name = Getattr(kw,"name");
   if (name && (Cmp(name,"noblock") == 0)) {
     char *cstr = Char(inputcode);
     size_t len = Len(inputcode);
     if (len && cstr[0] == '{') {
       --len; ++cstr; 
       if (len && cstr[len - 1] == '}') { --len; }
       /* we now remove the extra spaces */
       while (len && isspace((int)cstr[0])) { --len; ++cstr; }
       while (len && isspace((int)cstr[len - 1])) { --len; }
       modified_code = NewStringWithSize(cstr, len);
       break;
     }
   }
   kw = nextSibling(kw);
  }
  return modified_code;
}


static Node *nscope = 0;
static Node *nscope_inner = 0;

/* Remove the scope prefix from cname and return the base name without the prefix.
 * The scopes required for the symbol name are resolved and/or created, if required.
 * For example AA::BB::CC as input returns CC and creates the namespace AA then inner 
 * namespace BB in the current scope. If cname is found to already exist as a weak symbol
 * (forward reference) then the scope might be changed to match, such as when a symbol match 
 * is made via a using reference. */
static String *resolve_create_node_scope(String *cname) {
  Symtab *gscope = 0;
  Node *cname_node = 0;
  int skip_lookup = 0;
  nscope = 0;
  nscope_inner = 0;  

  if (Strncmp(cname,"::",2) == 0)
    skip_lookup = 1;

  cname_node = skip_lookup ? 0 : Swig_symbol_clookup_no_inherit(cname, 0);

  if (cname_node) {
    /* The symbol has been defined already or is in another scope.
       If it is a weak symbol, it needs replacing and if it was brought into the current scope
       via a using declaration, the scope needs adjusting appropriately for the new symbol.
       Similarly for defined templates. */
    Symtab *symtab = Getattr(cname_node, "sym:symtab");
    Node *sym_weak = Getattr(cname_node, "sym:weak");
    if ((symtab && sym_weak) || Equal(nodeType(cname_node), "template")) {
      /* Check if the scope is the current scope */
      String *current_scopename = Swig_symbol_qualifiedscopename(0);
      String *found_scopename = Swig_symbol_qualifiedscopename(symtab);
      int len;
      if (!current_scopename)
	current_scopename = NewString("");
      if (!found_scopename)
	found_scopename = NewString("");
      len = Len(current_scopename);
      if ((len > 0) && (Strncmp(current_scopename, found_scopename, len) == 0)) {
	if (Len(found_scopename) > len + 2) {
	  /* A matching weak symbol was found in non-global scope, some scope adjustment may be required */
	  String *new_cname = NewString(Char(found_scopename) + len + 2); /* skip over "::" prefix */
	  String *base = Swig_scopename_last(cname);
	  Printf(new_cname, "::%s", base);
	  cname = new_cname;
	  Delete(base);
	} else {
	  /* A matching weak symbol was found in the same non-global local scope, no scope adjustment required */
	  assert(len == Len(found_scopename));
	}
      } else {
	String *base = Swig_scopename_last(cname);
	if (Len(found_scopename) > 0) {
	  /* A matching weak symbol was found in a different scope to the local scope - probably via a using declaration */
	  cname = NewStringf("%s::%s", found_scopename, base);
	} else {
	  /* Either:
	      1) A matching weak symbol was found in a different scope to the local scope - this is actually a
	      symbol with the same name in a different scope which we don't want, so no adjustment required.
	      2) A matching weak symbol was found in the global scope - no adjustment required.
	  */
	  cname = Copy(base);
	}
	Delete(base);
      }
      Delete(current_scopename);
      Delete(found_scopename);
    }
  }

  if (Swig_scopename_check(cname)) {
    Node   *ns;
    String *prefix = Swig_scopename_prefix(cname);
    String *base = Swig_scopename_last(cname);
    if (prefix && (Strncmp(prefix,"::",2) == 0)) {
/* I don't think we can use :: global scope to declare classes and hence neither %template. - consider reporting error instead - wsfulton. */
      /* Use the global scope */
      String *nprefix = NewString(Char(prefix)+2);
      Delete(prefix);
      prefix= nprefix;
      gscope = set_scope_to_global();
    }
    if (Len(prefix) == 0) {
      /* Use the global scope, but we need to add a 'global' namespace.  */
      if (!gscope) gscope = set_scope_to_global();
      /* note that this namespace is not the "unnamed" one,
	 and we don't use Setattr(nscope,"name", ""),
	 because the unnamed namespace is private */
      nscope = new_node("namespace");
      Setattr(nscope,"symtab", gscope);;
      nscope_inner = nscope;
      return base;
    }
    /* Try to locate the scope */
    ns = Swig_symbol_clookup(prefix,0);
    if (!ns) {
      Swig_error(cparse_file,cparse_line,"Undefined scope '%s'\n", prefix);
    } else {
      Symtab *nstab = Getattr(ns,"symtab");
      if (!nstab) {
	Swig_error(cparse_file,cparse_line, "'%s' is not defined as a valid scope.\n", prefix);
	ns = 0;
      } else {
	/* Check if the node scope is the current scope */
	String *tname = Swig_symbol_qualifiedscopename(0);
	String *nname = Swig_symbol_qualifiedscopename(nstab);
	if (tname && (Strcmp(tname,nname) == 0)) {
	  ns = 0;
	  cname = base;
	}
	Delete(tname);
	Delete(nname);
      }
      if (ns) {
	/* we will try to create a new node using the namespaces we
	   can find in the scope name */
	List *scopes;
	String *sname;
	Iterator si;
	String *name = NewString(prefix);
	scopes = NewList();
	while (name) {
	  String *base = Swig_scopename_last(name);
	  String *tprefix = Swig_scopename_prefix(name);
	  Insert(scopes,0,base);
	  Delete(base);
	  Delete(name);
	  name = tprefix;
	}
	for (si = First(scopes); si.item; si = Next(si)) {
	  Node *ns1,*ns2;
	  sname = si.item;
	  ns1 = Swig_symbol_clookup(sname,0);
	  assert(ns1);
	  if (Strcmp(nodeType(ns1),"namespace") == 0) {
	    if (Getattr(ns1,"alias")) {
	      ns1 = Getattr(ns1,"namespace");
	    }
	  } else {
	    /* now this last part is a class */
	    si = Next(si);
	    /*  or a nested class tree, which is unrolled here */
	    for (; si.item; si = Next(si)) {
	      if (si.item) {
		Printf(sname,"::%s",si.item);
	      }
	    }
	    /* we get the 'inner' class */
	    nscope_inner = Swig_symbol_clookup(sname,0);
	    /* set the scope to the inner class */
	    Swig_symbol_setscope(Getattr(nscope_inner,"symtab"));
	    /* save the last namespace prefix */
	    Delete(Namespaceprefix);
	    Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	    /* and return the node name, including the inner class prefix */
	    break;
	  }
	  /* here we just populate the namespace tree as usual */
	  ns2 = new_node("namespace");
	  Setattr(ns2,"name",sname);
	  Setattr(ns2,"symtab", Getattr(ns1,"symtab"));
	  add_symbols(ns2);
	  Swig_symbol_setscope(Getattr(ns1,"symtab"));
	  Delete(Namespaceprefix);
	  Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	  if (nscope_inner) {
	    if (Getattr(nscope_inner,"symtab") != Getattr(ns2,"symtab")) {
	      appendChild(nscope_inner,ns2);
	      Delete(ns2);
	    }
	  }
	  nscope_inner = ns2;
	  if (!nscope) nscope = ns2;
	}
	cname = base;
	Delete(scopes);
      }
    }
    Delete(prefix);
  }

  return cname;
}
 
/* look for simple typedef name in typedef list */
static String *try_to_find_a_name_for_unnamed_structure(const char *storage, Node *decls) {
  String *name = 0;
  Node *n = decls;
  if (storage && (strcmp(storage, "typedef") == 0)) {
    for (; n; n = nextSibling(n)) {
      if (!Len(Getattr(n, "decl"))) {
	name = Copy(Getattr(n, "name"));
	break;
      }
    }
  }
  return name;
}

/* traverse copied tree segment, and update outer class links*/
static void update_nested_classes(Node *n)
{
  Node *c = firstChild(n);
  while (c) {
    if (Getattr(c, "nested:outer"))
      Setattr(c, "nested:outer", n);
    update_nested_classes(c);
    c = nextSibling(c);
  }
}

/* -----------------------------------------------------------------------------
 * nested_forward_declaration()
 * 
 * Nested struct handling for C++ code if the nested classes are disabled.
 * Create the nested class/struct/union as a forward declaration.
 * ----------------------------------------------------------------------------- */

static Node *nested_forward_declaration(const char *storage, const char *kind, String *sname, String *name, Node *cpp_opt_declarators) {
  Node *nn = 0;
  int warned = 0;

  if (sname) {
    /* Add forward declaration of the nested type */
    Node *n = new_node("classforward");
    Setattr(n, "kind", kind);
    Setattr(n, "name", sname);
    Setattr(n, "storage", storage);
    Setattr(n, "sym:weak", "1");
    add_symbols(n);
    nn = n;
  }

  /* Add any variable instances. Also add in any further typedefs of the nested type.
     Note that anonymous typedefs (eg typedef struct {...} a, b;) are treated as class forward declarations */
  if (cpp_opt_declarators) {
    int storage_typedef = (storage && (strcmp(storage, "typedef") == 0));
    int variable_of_anonymous_type = !sname && !storage_typedef;
    if (!variable_of_anonymous_type) {
      int anonymous_typedef = !sname && (storage && (strcmp(storage, "typedef") == 0));
      Node *n = cpp_opt_declarators;
      SwigType *type = name;
      while (n) {
	Setattr(n, "type", type);
	Setattr(n, "storage", storage);
	if (anonymous_typedef) {
	  Setattr(n, "nodeType", "classforward");
	  Setattr(n, "sym:weak", "1");
	}
	n = nextSibling(n);
      }
      add_symbols(cpp_opt_declarators);

      if (nn) {
	set_nextSibling(nn, cpp_opt_declarators);
      } else {
	nn = cpp_opt_declarators;
      }
    }
  }

  if (!currentOuterClass || !GetFlag(currentOuterClass, "nested")) {
    if (nn && Equal(nodeType(nn), "classforward")) {
      Node *n = nn;
      SWIG_WARN_NODE_BEGIN(n);
      Swig_warning(WARN_PARSE_NAMED_NESTED_CLASS, cparse_file, cparse_line,"Nested %s not currently supported (%s ignored)\n", kind, sname ? sname : name);
      SWIG_WARN_NODE_END(n);
      warned = 1;
    }

    if (!warned) {
      Swig_warning(WARN_PARSE_UNNAMED_NESTED_CLASS, cparse_file, cparse_line, "Nested %s not currently supported (ignored).\n", kind);
    }
  }

  return nn;
}


Node *Swig_cparse(File *f) {
  scanner_file(f);
  top = 0;
  yyparse();
  return top;
}

static void single_new_feature(const char *featurename, String *val, Hash *featureattribs, char *declaratorid, SwigType *type, ParmList *declaratorparms, String *qualifier) {
  String *fname;
  String *name;
  String *fixname;
  SwigType *t = Copy(type);

  /* Printf(stdout, "single_new_feature: [%s] [%s] [%s] [%s] [%s] [%s]\n", featurename, val, declaratorid, t, ParmList_str_defaultargs(declaratorparms), qualifier); */

  /* Warn about deprecated features */
  if (strcmp(featurename, "nestedworkaround") == 0)
    Swig_warning(WARN_DEPRECATED_NESTED_WORKAROUND, cparse_file, cparse_line, "The 'nestedworkaround' feature is deprecated.\n");

  fname = NewStringf("feature:%s",featurename);
  if (declaratorid) {
    fixname = feature_identifier_fix(declaratorid);
  } else {
    fixname = NewStringEmpty();
  }
  if (Namespaceprefix) {
    name = NewStringf("%s::%s",Namespaceprefix, fixname);
  } else {
    name = fixname;
  }

  if (declaratorparms) Setmeta(val,"parms",declaratorparms);
  if (!Len(t)) t = 0;
  if (t) {
    if (qualifier) SwigType_push(t,qualifier);
    if (SwigType_isfunction(t)) {
      SwigType *decl = SwigType_pop_function(t);
      if (SwigType_ispointer(t)) {
	String *nname = NewStringf("*%s",name);
	Swig_feature_set(Swig_cparse_features(), nname, decl, fname, val, featureattribs);
	Delete(nname);
      } else {
	Swig_feature_set(Swig_cparse_features(), name, decl, fname, val, featureattribs);
      }
      Delete(decl);
    } else if (SwigType_ispointer(t)) {
      String *nname = NewStringf("*%s",name);
      Swig_feature_set(Swig_cparse_features(),nname,0,fname,val, featureattribs);
      Delete(nname);
    }
  } else {
    /* Global feature, that is, feature not associated with any particular symbol */
    Swig_feature_set(Swig_cparse_features(),name,0,fname,val, featureattribs);
  }
  Delete(fname);
  Delete(name);
}

/* Add a new feature to the Hash. Additional features are added if the feature has a parameter list (declaratorparms)
 * and one or more of the parameters have a default argument. An extra feature is added for each defaulted parameter,
 * simulating the equivalent overloaded method. */
static void new_feature(const char *featurename, String *val, Hash *featureattribs, char *declaratorid, SwigType *type, ParmList *declaratorparms, String *qualifier) {

  ParmList *declparms = declaratorparms;

  /* remove the { and } braces if the noblock attribute is set */
  String *newval = remove_block(featureattribs, val);
  val = newval ? newval : val;

  /* Add the feature */
  single_new_feature(featurename, val, featureattribs, declaratorid, type, declaratorparms, qualifier);

  /* Add extra features if there are default parameters in the parameter list */
  if (type) {
    while (declparms) {
      if (ParmList_has_defaultargs(declparms)) {

        /* Create a parameter list for the new feature by copying all
           but the last (defaulted) parameter */
        ParmList* newparms = CopyParmListMax(declparms, ParmList_len(declparms)-1);

        /* Create new declaration - with the last parameter removed */
        SwigType *newtype = Copy(type);
        Delete(SwigType_pop_function(newtype)); /* remove the old parameter list from newtype */
        SwigType_add_function(newtype,newparms);

        single_new_feature(featurename, Copy(val), featureattribs, declaratorid, newtype, newparms, qualifier);
        declparms = newparms;
      } else {
        declparms = 0;
      }
    }
  }
}

/* check if a function declaration is a plain C object */
static int is_cfunction(Node *n) {
  if (!cparse_cplusplus || cparse_externc)
    return 1;
  if (Swig_storage_isexternc(n)) {
    return 1;
  }
  return 0;
}

/* If the Node is a function with parameters, check to see if any of the parameters
 * have default arguments. If so create a new function for each defaulted argument. 
 * The additional functions form a linked list of nodes with the head being the original Node n. */
static void default_arguments(Node *n) {
  Node *function = n;

  if (function) {
    ParmList *varargs = Getattr(function,"feature:varargs");
    if (varargs) {
      /* Handles the %varargs directive by looking for "feature:varargs" and 
       * substituting ... with an alternative set of arguments.  */
      Parm     *p = Getattr(function,"parms");
      Parm     *pp = 0;
      while (p) {
	SwigType *t = Getattr(p,"type");
	if (Strcmp(t,"v(...)") == 0) {
	  if (pp) {
	    ParmList *cv = Copy(varargs);
	    set_nextSibling(pp,cv);
	    Delete(cv);
	  } else {
	    ParmList *cv =  Copy(varargs);
	    Setattr(function,"parms", cv);
	    Delete(cv);
	  }
	  break;
	}
	pp = p;
	p = nextSibling(p);
      }
    }

    /* Do not add in functions if kwargs is being used or if user wants old default argument wrapping
       (one wrapped method per function irrespective of number of default arguments) */
    if (compact_default_args 
	|| is_cfunction(function) 
	|| GetFlag(function,"feature:compactdefaultargs") 
	|| (GetFlag(function,"feature:kwargs") && kwargs_supported)) {
      ParmList *p = Getattr(function,"parms");
      if (p) 
        Setattr(p,"compactdefargs", "1"); /* mark parameters for special handling */
      function = 0; /* don't add in extra methods */
    }
  }

  while (function) {
    ParmList *parms = Getattr(function,"parms");
    if (ParmList_has_defaultargs(parms)) {

      /* Create a parameter list for the new function by copying all
         but the last (defaulted) parameter */
      ParmList* newparms = CopyParmListMax(parms,ParmList_len(parms)-1);

      /* Create new function and add to symbol table */
      {
	SwigType *ntype = Copy(nodeType(function));
	char *cntype = Char(ntype);
        Node *new_function = new_node(ntype);
        SwigType *decl = Copy(Getattr(function,"decl"));
        int constqualifier = SwigType_isconst(decl);
	String *ccode = Copy(Getattr(function,"code"));
	String *cstorage = Copy(Getattr(function,"storage"));
	String *cvalue = Copy(Getattr(function,"value"));
	SwigType *ctype = Copy(Getattr(function,"type"));
	String *cthrow = Copy(Getattr(function,"throw"));

        Delete(SwigType_pop_function(decl)); /* remove the old parameter list from decl */
        SwigType_add_function(decl,newparms);
        if (constqualifier)
          SwigType_add_qualifier(decl,"const");

        Setattr(new_function,"name", Getattr(function,"name"));
        Setattr(new_function,"code", ccode);
        Setattr(new_function,"decl", decl);
        Setattr(new_function,"parms", newparms);
        Setattr(new_function,"storage", cstorage);
        Setattr(new_function,"value", cvalue);
        Setattr(new_function,"type", ctype);
        Setattr(new_function,"throw", cthrow);

	Delete(ccode);
	Delete(cstorage);
	Delete(cvalue);
	Delete(ctype);
	Delete(cthrow);
	Delete(decl);

        {
          Node *throws = Getattr(function,"throws");
	  ParmList *pl = CopyParmList(throws);
          if (throws) Setattr(new_function,"throws",pl);
	  Delete(pl);
        }

        /* copy specific attributes for global (or in a namespace) template functions - these are not templated class methods */
        if (strcmp(cntype,"template") == 0) {
          Node *templatetype = Getattr(function,"templatetype");
          Node *symtypename = Getattr(function,"sym:typename");
          Parm *templateparms = Getattr(function,"templateparms");
          if (templatetype) {
	    Node *tmp = Copy(templatetype);
	    Setattr(new_function,"templatetype",tmp);
	    Delete(tmp);
	  }
          if (symtypename) {
	    Node *tmp = Copy(symtypename);
	    Setattr(new_function,"sym:typename",tmp);
	    Delete(tmp);
	  }
          if (templateparms) {
	    Parm *tmp = CopyParmList(templateparms);
	    Setattr(new_function,"templateparms",tmp);
	    Delete(tmp);
	  }
        } else if (strcmp(cntype,"constructor") == 0) {
          /* only copied for constructors as this is not a user defined feature - it is hard coded in the parser */
          if (GetFlag(function,"feature:new")) SetFlag(new_function,"feature:new");
        }

        add_symbols(new_function);
        /* mark added functions as ones with overloaded parameters and point to the parsed method */
        Setattr(new_function,"defaultargs", n);

        /* Point to the new function, extending the linked list */
        set_nextSibling(function, new_function);
	Delete(new_function);
        function = new_function;
	
	Delete(ntype);
      }
    } else {
      function = 0;
    }
  }
}

/* -----------------------------------------------------------------------------
 * mark_nodes_as_extend()
 *
 * Used by the %extend to mark subtypes with "feature:extend".
 * template instances declared within %extend are skipped
 * ----------------------------------------------------------------------------- */

static void mark_nodes_as_extend(Node *n) {
  for (; n; n = nextSibling(n)) {
    if (Getattr(n, "template") && Strcmp(nodeType(n), "class") == 0)
      continue;
    /* Fix me: extend is not a feature. Replace with isextendmember? */
    Setattr(n, "feature:extend", "1");
    mark_nodes_as_extend(firstChild(n));
  }
}


#line 1343 "y.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    ID = 258,
    HBLOCK = 259,
    POUND = 260,
    STRING = 261,
    WSTRING = 262,
    INCLUDE = 263,
    IMPORT = 264,
    INSERT = 265,
    CHARCONST = 266,
    WCHARCONST = 267,
    NUM_INT = 268,
    NUM_FLOAT = 269,
    NUM_UNSIGNED = 270,
    NUM_LONG = 271,
    NUM_ULONG = 272,
    NUM_LONGLONG = 273,
    NUM_ULONGLONG = 274,
    NUM_BOOL = 275,
    TYPEDEF = 276,
    TYPE_INT = 277,
    TYPE_UNSIGNED = 278,
    TYPE_SHORT = 279,
    TYPE_LONG = 280,
    TYPE_FLOAT = 281,
    TYPE_DOUBLE = 282,
    TYPE_CHAR = 283,
    TYPE_WCHAR = 284,
    TYPE_VOID = 285,
    TYPE_SIGNED = 286,
    TYPE_BOOL = 287,
    TYPE_COMPLEX = 288,
    TYPE_TYPEDEF = 289,
    TYPE_RAW = 290,
    TYPE_NON_ISO_INT8 = 291,
    TYPE_NON_ISO_INT16 = 292,
    TYPE_NON_ISO_INT32 = 293,
    TYPE_NON_ISO_INT64 = 294,
    LPAREN = 295,
    RPAREN = 296,
    COMMA = 297,
    SEMI = 298,
    EXTERN = 299,
    INIT = 300,
    LBRACE = 301,
    RBRACE = 302,
    PERIOD = 303,
    CONST_QUAL = 304,
    VOLATILE = 305,
    REGISTER = 306,
    STRUCT = 307,
    UNION = 308,
    EQUAL = 309,
    SIZEOF = 310,
    MODULE = 311,
    LBRACKET = 312,
    RBRACKET = 313,
    BEGINFILE = 314,
    ENDOFFILE = 315,
    ILLEGAL = 316,
    CONSTANT = 317,
    NAME = 318,
    RENAME = 319,
    NAMEWARN = 320,
    EXTEND = 321,
    PRAGMA = 322,
    FEATURE = 323,
    VARARGS = 324,
    ENUM = 325,
    CLASS = 326,
    TYPENAME = 327,
    PRIVATE = 328,
    PUBLIC = 329,
    PROTECTED = 330,
    COLON = 331,
    STATIC = 332,
    VIRTUAL = 333,
    FRIEND = 334,
    THROW = 335,
    CATCH = 336,
    EXPLICIT = 337,
    STATIC_ASSERT = 338,
    CONSTEXPR = 339,
    THREAD_LOCAL = 340,
    DECLTYPE = 341,
    AUTO = 342,
    NOEXCEPT = 343,
    OVERRIDE = 344,
    FINAL = 345,
    USING = 346,
    NAMESPACE = 347,
    NATIVE = 348,
    INLINE = 349,
    TYPEMAP = 350,
    EXCEPT = 351,
    ECHO = 352,
    APPLY = 353,
    CLEAR = 354,
    SWIGTEMPLATE = 355,
    FRAGMENT = 356,
    WARN = 357,
    LESSTHAN = 358,
    GREATERTHAN = 359,
    DELETE_KW = 360,
    DEFAULT = 361,
    LESSTHANOREQUALTO = 362,
    GREATERTHANOREQUALTO = 363,
    EQUALTO = 364,
    NOTEQUALTO = 365,
    ARROW = 366,
    QUESTIONMARK = 367,
    TYPES = 368,
    PARMS = 369,
    NONID = 370,
    DSTAR = 371,
    DCNOT = 372,
    TEMPLATE = 373,
    OPERATOR = 374,
    COPERATOR = 375,
    PARSETYPE = 376,
    PARSEPARM = 377,
    PARSEPARMS = 378,
    CAST = 379,
    LOR = 380,
    LAND = 381,
    OR = 382,
    XOR = 383,
    AND = 384,
    LSHIFT = 385,
    RSHIFT = 386,
    PLUS = 387,
    MINUS = 388,
    STAR = 389,
    SLASH = 390,
    MODULO = 391,
    UMINUS = 392,
    NOT = 393,
    LNOT = 394,
    DCOLON = 395
  };
#endif
/* Tokens.  */
#define ID 258
#define HBLOCK 259
#define POUND 260
#define STRING 261
#define WSTRING 262
#define INCLUDE 263
#define IMPORT 264
#define INSERT 265
#define CHARCONST 266
#define WCHARCONST 267
#define NUM_INT 268
#define NUM_FLOAT 269
#define NUM_UNSIGNED 270
#define NUM_LONG 271
#define NUM_ULONG 272
#define NUM_LONGLONG 273
#define NUM_ULONGLONG 274
#define NUM_BOOL 275
#define TYPEDEF 276
#define TYPE_INT 277
#define TYPE_UNSIGNED 278
#define TYPE_SHORT 279
#define TYPE_LONG 280
#define TYPE_FLOAT 281
#define TYPE_DOUBLE 282
#define TYPE_CHAR 283
#define TYPE_WCHAR 284
#define TYPE_VOID 285
#define TYPE_SIGNED 286
#define TYPE_BOOL 287
#define TYPE_COMPLEX 288
#define TYPE_TYPEDEF 289
#define TYPE_RAW 290
#define TYPE_NON_ISO_INT8 291
#define TYPE_NON_ISO_INT16 292
#define TYPE_NON_ISO_INT32 293
#define TYPE_NON_ISO_INT64 294
#define LPAREN 295
#define RPAREN 296
#define COMMA 297
#define SEMI 298
#define EXTERN 299
#define INIT 300
#define LBRACE 301
#define RBRACE 302
#define PERIOD 303
#define CONST_QUAL 304
#define VOLATILE 305
#define REGISTER 306
#define STRUCT 307
#define UNION 308
#define EQUAL 309
#define SIZEOF 310
#define MODULE 311
#define LBRACKET 312
#define RBRACKET 313
#define BEGINFILE 314
#define ENDOFFILE 315
#define ILLEGAL 316
#define CONSTANT 317
#define NAME 318
#define RENAME 319
#define NAMEWARN 320
#define EXTEND 321
#define PRAGMA 322
#define FEATURE 323
#define VARARGS 324
#define ENUM 325
#define CLASS 326
#define TYPENAME 327
#define PRIVATE 328
#define PUBLIC 329
#define PROTECTED 330
#define COLON 331
#define STATIC 332
#define VIRTUAL 333
#define FRIEND 334
#define THROW 335
#define CATCH 336
#define EXPLICIT 337
#define STATIC_ASSERT 338
#define CONSTEXPR 339
#define THREAD_LOCAL 340
#define DECLTYPE 341
#define AUTO 342
#define NOEXCEPT 343
#define OVERRIDE 344
#define FINAL 345
#define USING 346
#define NAMESPACE 347
#define NATIVE 348
#define INLINE 349
#define TYPEMAP 350
#define EXCEPT 351
#define ECHO 352
#define APPLY 353
#define CLEAR 354
#define SWIGTEMPLATE 355
#define FRAGMENT 356
#define WARN 357
#define LESSTHAN 358
#define GREATERTHAN 359
#define DELETE_KW 360
#define DEFAULT 361
#define LESSTHANOREQUALTO 362
#define GREATERTHANOREQUALTO 363
#define EQUALTO 364
#define NOTEQUALTO 365
#define ARROW 366
#define QUESTIONMARK 367
#define TYPES 368
#define PARMS 369
#define NONID 370
#define DSTAR 371
#define DCNOT 372
#define TEMPLATE 373
#define OPERATOR 374
#define COPERATOR 375
#define PARSETYPE 376
#define PARSEPARM 377
#define PARSEPARMS 378
#define CAST 379
#define LOR 380
#define LAND 381
#define OR 382
#define XOR 383
#define AND 384
#define LSHIFT 385
#define RSHIFT 386
#define PLUS 387
#define MINUS 388
#define STAR 389
#define SLASH 390
#define MODULO 391
#define UMINUS 392
#define NOT 393
#define LNOT 394
#define DCOLON 395

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{
#line 1299 "parser.y" /* yacc.c:355  */

  char  *id;
  List  *bases;
  struct Define {
    String *val;
    String *rawval;
    int     type;
    String *qualifier;
    String *bitfield;
    Parm   *throws;
    String *throwf;
    String *nexcept;
  } dtype;
  struct {
    const char *type;
    String *filename;
    int   line;
  } loc;
  struct {
    char      *id;
    SwigType  *type;
    String    *defarg;
    ParmList  *parms;
    short      have_parms;
    ParmList  *throws;
    String    *throwf;
    String    *nexcept;
  } decl;
  Parm         *tparms;
  struct {
    String     *method;
    Hash       *kwargs;
  } tmap;
  struct {
    String     *type;
    String     *us;
  } ptype;
  SwigType     *type;
  String       *str;
  Parm         *p;
  ParmList     *pl;
  int           intvalue;
  Node         *node;

#line 1708 "y.tab.c" /* yacc.c:355  */
};
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 1723 "y.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  61
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   5010

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  141
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  167
/* YYNRULES -- Number of rules.  */
#define YYNRULES  572
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1117

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   395

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  1464,  1464,  1476,  1480,  1483,  1486,  1489,  1492,  1497,
    1502,  1507,  1508,  1509,  1510,  1511,  1521,  1537,  1547,  1548,
    1549,  1550,  1551,  1552,  1553,  1554,  1555,  1556,  1557,  1558,
    1559,  1560,  1561,  1562,  1563,  1564,  1565,  1566,  1567,  1574,
    1574,  1656,  1666,  1677,  1698,  1720,  1731,  1740,  1759,  1765,
    1771,  1776,  1783,  1790,  1794,  1807,  1816,  1831,  1844,  1844,
    1900,  1901,  1908,  1927,  1958,  1962,  1972,  1977,  1995,  2038,
    2044,  2057,  2063,  2089,  2095,  2102,  2103,  2106,  2107,  2114,
    2160,  2206,  2217,  2220,  2247,  2253,  2259,  2265,  2273,  2279,
    2285,  2291,  2299,  2300,  2301,  2304,  2309,  2319,  2355,  2356,
    2391,  2408,  2416,  2429,  2454,  2460,  2464,  2467,  2478,  2483,
    2496,  2508,  2798,  2808,  2815,  2816,  2820,  2820,  2845,  2851,
    2862,  2878,  2938,  2996,  3000,  3023,  3027,  3038,  3045,  3052,
    3059,  3068,  3069,  3070,  3071,  3072,  3073,  3074,  3085,  3090,
    3095,  3102,  3108,  3113,  3116,  3116,  3129,  3132,  3135,  3144,
    3147,  3154,  3176,  3205,  3303,  3355,  3356,  3357,  3358,  3359,
    3360,  3365,  3365,  3612,  3612,  3757,  3758,  3770,  3788,  3788,
    4047,  4053,  4059,  4062,  4065,  4068,  4071,  4074,  4077,  4082,
    4118,  4122,  4125,  4128,  4133,  4137,  4142,  4152,  4183,  4183,
    4212,  4212,  4234,  4261,  4278,  4283,  4278,  4291,  4292,  4293,
    4293,  4309,  4310,  4327,  4328,  4329,  4330,  4331,  4332,  4333,
    4334,  4335,  4336,  4337,  4338,  4339,  4340,  4341,  4342,  4343,
    4352,  4380,  4407,  4438,  4453,  4470,  4488,  4507,  4526,  4533,
    4540,  4547,  4555,  4563,  4566,  4570,  4573,  4574,  4575,  4576,
    4577,  4578,  4579,  4580,  4583,  4590,  4597,  4606,  4615,  4624,
    4636,  4639,  4642,  4643,  4644,  4645,  4647,  4656,  4657,  4667,
    4677,  4678,  4679,  4680,  4681,  4682,  4683,  4684,  4685,  4686,
    4687,  4688,  4689,  4690,  4691,  4692,  4699,  4710,  4714,  4717,
    4721,  4725,  4735,  4743,  4751,  4764,  4768,  4771,  4775,  4779,
    4807,  4815,  4827,  4842,  4852,  4861,  4872,  4876,  4880,  4887,
    4904,  4921,  4929,  4937,  4946,  4955,  4959,  4968,  4979,  4990,
    5002,  5012,  5026,  5034,  5043,  5052,  5056,  5065,  5076,  5087,
    5099,  5109,  5119,  5130,  5143,  5150,  5158,  5174,  5182,  5193,
    5204,  5215,  5234,  5242,  5259,  5267,  5274,  5281,  5292,  5303,
    5314,  5334,  5355,  5361,  5367,  5374,  5381,  5390,  5399,  5402,
    5411,  5420,  5427,  5434,  5441,  5451,  5462,  5473,  5484,  5491,
    5498,  5501,  5518,  5528,  5535,  5541,  5546,  5552,  5556,  5562,
    5563,  5564,  5570,  5576,  5580,  5581,  5585,  5592,  5595,  5596,
    5597,  5598,  5599,  5601,  5604,  5607,  5612,  5623,  5648,  5651,
    5705,  5709,  5713,  5717,  5721,  5725,  5729,  5733,  5737,  5741,
    5745,  5749,  5753,  5757,  5763,  5763,  5777,  5793,  5796,  5802,
    5815,  5829,  5830,  5833,  5834,  5838,  5844,  5847,  5851,  5856,
    5864,  5876,  5891,  5892,  5911,  5912,  5916,  5921,  5926,  5927,
    5932,  5945,  5960,  5967,  5984,  5991,  5998,  6005,  6013,  6021,
    6025,  6029,  6035,  6036,  6037,  6038,  6039,  6040,  6041,  6042,
    6045,  6049,  6053,  6057,  6061,  6065,  6069,  6073,  6077,  6081,
    6085,  6089,  6093,  6097,  6111,  6115,  6119,  6125,  6129,  6133,
    6137,  6141,  6157,  6162,  6165,  6170,  6175,  6175,  6176,  6179,
    6196,  6205,  6205,  6223,  6223,  6241,  6242,  6243,  6247,  6251,
    6255,  6259,  6265,  6268,  6272,  6278,  6279,  6282,  6285,  6288,
    6291,  6296,  6301,  6306,  6311,  6316,  6323,  6329,  6333,  6337,
    6345,  6353,  6361,  6370,  6379,  6386,  6395,  6396,  6399,  6400,
    6401,  6402,  6405,  6417,  6423,  6429,  6433,  6434,  6435,  6438,
    6439,  6440,  6443,  6444,  6447,  6452,  6456,  6459,  6462,  6465,
    6470,  6474,  6477,  6484,  6490,  6499,  6504,  6508,  6511,  6514,
    6517,  6522,  6526,  6529,  6532,  6538,  6543,  6546,  6558,  6561,
    6564,  6568,  6573,  6586,  6590,  6595,  6601,  6605,  6610,  6614,
    6621,  6624,  6629
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "ID", "HBLOCK", "POUND", "STRING",
  "WSTRING", "INCLUDE", "IMPORT", "INSERT", "CHARCONST", "WCHARCONST",
  "NUM_INT", "NUM_FLOAT", "NUM_UNSIGNED", "NUM_LONG", "NUM_ULONG",
  "NUM_LONGLONG", "NUM_ULONGLONG", "NUM_BOOL", "TYPEDEF", "TYPE_INT",
  "TYPE_UNSIGNED", "TYPE_SHORT", "TYPE_LONG", "TYPE_FLOAT", "TYPE_DOUBLE",
  "TYPE_CHAR", "TYPE_WCHAR", "TYPE_VOID", "TYPE_SIGNED", "TYPE_BOOL",
  "TYPE_COMPLEX", "TYPE_TYPEDEF", "TYPE_RAW", "TYPE_NON_ISO_INT8",
  "TYPE_NON_ISO_INT16", "TYPE_NON_ISO_INT32", "TYPE_NON_ISO_INT64",
  "LPAREN", "RPAREN", "COMMA", "SEMI", "EXTERN", "INIT", "LBRACE",
  "RBRACE", "PERIOD", "CONST_QUAL", "VOLATILE", "REGISTER", "STRUCT",
  "UNION", "EQUAL", "SIZEOF", "MODULE", "LBRACKET", "RBRACKET",
  "BEGINFILE", "ENDOFFILE", "ILLEGAL", "CONSTANT", "NAME", "RENAME",
  "NAMEWARN", "EXTEND", "PRAGMA", "FEATURE", "VARARGS", "ENUM", "CLASS",
  "TYPENAME", "PRIVATE", "PUBLIC", "PROTECTED", "COLON", "STATIC",
  "VIRTUAL", "FRIEND", "THROW", "CATCH", "EXPLICIT", "STATIC_ASSERT",
  "CONSTEXPR", "THREAD_LOCAL", "DECLTYPE", "AUTO", "NOEXCEPT", "OVERRIDE",
  "FINAL", "USING", "NAMESPACE", "NATIVE", "INLINE", "TYPEMAP", "EXCEPT",
  "ECHO", "APPLY", "CLEAR", "SWIGTEMPLATE", "FRAGMENT", "WARN", "LESSTHAN",
  "GREATERTHAN", "DELETE_KW", "DEFAULT", "LESSTHANOREQUALTO",
  "GREATERTHANOREQUALTO", "EQUALTO", "NOTEQUALTO", "ARROW", "QUESTIONMARK",
  "TYPES", "PARMS", "NONID", "DSTAR", "DCNOT", "TEMPLATE", "OPERATOR",
  "COPERATOR", "PARSETYPE", "PARSEPARM", "PARSEPARMS", "CAST", "LOR",
  "LAND", "OR", "XOR", "AND", "LSHIFT", "RSHIFT", "PLUS", "MINUS", "STAR",
  "SLASH", "MODULO", "UMINUS", "NOT", "LNOT", "DCOLON", "$accept",
  "program", "interface", "declaration", "swig_directive",
  "extend_directive", "$@1", "apply_directive", "clear_directive",
  "constant_directive", "echo_directive", "except_directive", "stringtype",
  "fname", "fragment_directive", "include_directive", "$@2", "includetype",
  "inline_directive", "insert_directive", "module_directive",
  "name_directive", "native_directive", "pragma_directive", "pragma_arg",
  "pragma_lang", "rename_directive", "rename_namewarn",
  "feature_directive", "stringbracesemi", "featattr", "varargs_directive",
  "varargs_parms", "typemap_directive", "typemap_type", "tm_list",
  "tm_tail", "typemap_parm", "types_directive", "template_directive",
  "warn_directive", "c_declaration", "$@3", "c_decl", "c_decl_tail",
  "initializer", "cpp_alternate_rettype", "cpp_lambda_decl",
  "lambda_introducer", "lambda_body", "lambda_tail", "$@4", "c_enum_key",
  "c_enum_inherit", "c_enum_forward_decl", "c_enum_decl",
  "c_constructor_decl", "cpp_declaration", "cpp_class_decl", "@5", "@6",
  "cpp_opt_declarators", "cpp_forward_class_decl", "cpp_template_decl",
  "$@7", "cpp_temp_possible", "template_parms", "templateparameters",
  "templateparameter", "templateparameterstail", "cpp_using_decl",
  "cpp_namespace_decl", "$@8", "$@9", "cpp_members", "$@10", "$@11",
  "$@12", "cpp_member", "cpp_constructor_decl", "cpp_destructor_decl",
  "cpp_conversion_operator", "cpp_catch_decl", "cpp_static_assert",
  "cpp_protection_decl", "cpp_swig_directive", "cpp_end", "cpp_vend",
  "anonymous_bitfield", "anon_bitfield_type", "storage_class", "parms",
  "rawparms", "ptail", "parm", "valparms", "rawvalparms", "valptail",
  "valparm", "def_args", "parameter_declarator",
  "typemap_parameter_declarator", "declarator", "notso_direct_declarator",
  "direct_declarator", "abstract_declarator", "direct_abstract_declarator",
  "pointer", "type_qualifier", "type_qualifier_raw", "type", "rawtype",
  "type_right", "decltype", "primitive_type", "primitive_type_list",
  "type_specifier", "definetype", "$@13", "default_delete",
  "deleted_definition", "explicit_default", "ename",
  "optional_constant_directive", "enumlist", "edecl", "etype", "expr",
  "valexpr", "exprnum", "exprcompound", "ellipsis", "variadic", "inherit",
  "raw_inherit", "$@14", "base_list", "base_specifier", "@15", "@16",
  "access_specifier", "templcpptype", "cpptype", "opt_virtual",
  "virt_specifier_seq", "exception_specification", "cpp_const", "ctor_end",
  "ctor_initializer", "mem_initializer_list", "mem_initializer",
  "template_decl", "identifier", "idstring", "idstringopt", "idcolon",
  "idcolontail", "idtemplate", "idcolonnt", "idcolontailnt", "string",
  "wstring", "stringbrace", "options", "kwargs", "stringnum", "empty", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395
};
# endif

#define YYPACT_NINF -953

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-953)))

#define YYTABLE_NINF -573

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     459,  4104,  4176,   185,    61,  3594,  -953,  -953,  -953,  -953,
    -953,  -953,  -953,  -953,  -953,  -953,  -953,  -953,  -953,  -953,
    -953,   100,  -953,  -953,  -953,  -953,  -953,    81,  -953,  -953,
    -953,  -953,  -953,    22,    95,   192,   125,  -953,  -953,    82,
     180,  -953,    55,   253,  4821,   676,   401,   676,  -953,  -953,
    -953,  2743,  -953,    55,   100,  -953,    89,  -953,   300,   314,
    4538,  -953,   252,  -953,  -953,  -953,   348,  -953,  -953,    28,
     360,  4248,   381,  -953,  -953,   360,   389,   414,   426,   550,
    -953,  -953,   440,   423,   449,   172,   367,  1069,   471,   257,
     475,   569,   439,  4609,  4609,   481,   488,   539,   492,   315,
    -953,  -953,  -953,  -953,  -953,  -953,  -953,  -953,  -953,  -953,
     360,  -953,  -953,  -953,  -953,  -953,  -953,  -953,  1214,  -953,
    -953,  -953,  -953,  -953,  -953,  -953,  -953,  -953,  -953,  -953,
    -953,  -953,  -953,  -953,  -953,  -953,  -953,  -953,  4680,  -953,
    2122,  -953,  -953,   503,  -953,  -953,   522,   547,    55,    52,
     380,  -953,  -953,   676,  -953,  3266,   551,     3,  2256,  3060,
     608,   152,   195,   204,    55,  -953,  -953,   290,    48,   290,
     175,   799,   487,  -953,  -953,  -953,  -953,  -953,   101,   169,
    -953,  -953,  -953,   576,  -953,   588,  -953,  -953,   407,  -953,
    -953,   380,    75,   407,   407,  -953,   629,  1499,  -953,   154,
    1808,    55,   101,   101,  -953,   407,  4466,  -953,  -953,  4538,
    -953,  -953,  -953,  -953,  -953,    55,   336,  -953,   208,   640,
     101,  -953,  -953,   407,   101,  -953,  -953,  -953,   705,  4538,
     670,   345,   690,   702,   407,   539,   705,  4538,  4538,    55,
     539,  1546,   357,   433,   407,  1001,   642,  1499,    55,  1618,
     768,  -953,  -953,  -953,  -953,  -953,  -953,  -953,  -953,  -953,
    -953,  -953,  3060,   269,  3060,  3060,  3060,  3060,  3060,  3060,
    3060,  -953,   655,  -953,   720,   728,   288,  2071,    20,  -953,
    -953,   705,   757,  -953,  -953,   727,   731,   740,  -953,    89,
     668,  -953,  3379,  1129,  1129,   742,   744,  1670,   671,   741,
    -953,  -953,  -953,   735,  3060,  -953,  -953,  -953,  -953,  4252,
    -953,  2071,   758,  3379,   756,    55,   341,   175,  -953,   763,
     341,   175,  -953,   672,  -953,  -953,  4538,  2390,  -953,  4538,
    2524,   765,   834,  1056,   341,   175,   699,  1735,  -953,  -953,
      89,   776,  4538,  -953,  -953,  -953,  -953,   780,   705,    55,
    -953,  -953,  -953,   214,   781,  -953,  -953,   499,   290,   560,
    -953,   790,   787,   794,   784,   641,   804,   800,  -953,   814,
     816,  -953,  -953,  -953,    55,  -953,   819,   829,  -953,   831,
     832,  4609,  -953,  -953,  -953,  -953,  -953,  4609,  -953,  -953,
    -953,   836,  -953,  -953,   679,   190,   837,   786,  -953,   838,
    -953,    69,  -953,  -953,    60,   489,   489,   489,   266,   777,
     859,   174,   858,  1236,  1872,   792,  1735,   801,    40,   839,
     251,  -953,  3451,  1888,  -953,   867,  -953,   277,  -953,  2224,
    4751,   869,  3009,  2231,  -953,  -953,  -953,  -953,  -953,  -953,
    2122,  -953,  -953,  -953,  3060,  3060,  3060,  3060,  3060,  3060,
    3060,  3060,  3060,  3060,  3060,  3060,  3060,  3060,  3060,  3060,
    3060,  -953,  -953,  -953,  -953,  -953,   380,   354,   354,  3268,
     810,   306,  -953,   444,  -953,  -953,   354,   354,   448,   818,
     489,   489,  3060,  2071,  -953,  4538,  1755,     9,   872,  -953,
    4538,  2658,   883,  -953,   891,  -953,  4613,   894,  -953,  4700,
     888,   892,   341,   175,   896,   341,   175,  1665,   897,   898,
    2128,   341,  -953,  -953,   588,   295,  -953,  -953,   407,  2084,
    -953,   899,   915,  -953,  -953,  -953,   724,  1247,  1407,   919,
    4538,  1499,   918,  -953,  3696,   924,  -953,  1564,  4609,   419,
     933,   929,   702,   501,   935,   407,  4538,    36,   889,  4538,
    -953,  -953,  -953,   489,  1108,  1061,    16,  -953,  1828,  4891,
     925,  4821,   432,  -953,   940,   777,   943,   286,   900,   895,
     181,  -953,   949,  -953,   290,   908,  -953,  -953,   942,  3060,
    2792,  2926,  3194,   143,   401,   945,   720,   694,   694,   953,
     953,  2624,  2875,  3009,  2499,  2365,  2231,   717,   717,   674,
     674,  -953,  -953,  -953,    55,   818,  -953,  -953,  -953,  -953,
     354,   468,    48,  4825,   950,   526,   818,  -953,  1061,  1061,
     956,  -953,  4837,  1061,  -953,  -953,  -953,  -953,  1061,   952,
     957,   960,   961,  2210,   341,   175,   967,   968,   973,   341,
    -953,  -953,  -953,   705,  3798,  -953,   959,  -953,   190,   980,
    -953,  -953,  1994,  -953,  -953,   705,  -953,  -953,  -953,   983,
    -953,   815,   705,  -953,   970,    53,   698,  1247,  -953,   815,
    -953,  -953,  -953,  3900,    45,  4751,   385,  -953,  -953,  4538,
    -953,  -953,   886,  -953,   104,   927,  -953,   986,   985,  -953,
      55,  1449,   838,  -953,   815,    74,  1061,  -953,  -953,   100,
    -953,  1888,  -953,  -953,  -953,  -953,   369,  -953,  -953,   972,
    1717,  4538,  3060,  -953,  -953,  -953,  -953,  1499,  -953,  -953,
    -953,  -953,   290,  -953,  -953,  1002,  -953,   811,  -953,  1994,
    -953,  2071,  3060,  3060,  3194,  3521,  3060,  1004,  1006,  1012,
    1003,  -953,  3060,   290,  -953,  -953,  -953,  -953,   613,   341,
    -953,  -953,   341,   341,  1061,  1061,  1007,  1008,  1009,   341,
    1061,  1014,  1018,  -953,   407,   407,  -953,  -953,  1021,   978,
     995,   997,   936,  1035,   101,  -953,  -953,  -953,  -953,  -953,
    -953,  -953,  -953,  -953,  -953,  -953,  -953,  -953,  -953,  -953,
    -953,  -953,  -953,  -953,  -953,  -953,  1031,  1994,  -953,  -953,
    -953,  -953,  -953,  -953,  -953,  -953,  4319,  1032,  4538,   600,
    -953,    36,  -953,  2084,  1317,   407,  1040,  -953,   815,  1052,
    -953,    77,  1499,    17,  -953,  4609,  -953,  1059,   176,   101,
     216,  -953,  2122,   273,  -953,  1051,    28,   447,  -953,  -953,
    -953,  -953,  -953,  -953,  -953,  -953,  4391,  -953,  4002,  1063,
    -953,  -953,   181,  4538,  -953,   520,  -953,   101,   502,  -953,
    4538,   560,  1053,  1046,  -953,  1067,  2357,  1888,  -953,   908,
    -953,  -953,  -953,    55,  -953,  1076,  1994,  2071,  2071,  2071,
    3060,  3060,  -953,  4751,  2741,  -953,  -953,   341,   341,  1061,
    1081,  1083,   341,  1061,  1061,  -953,  -953,  1994,  -953,  -953,
    -953,  -953,   101,  -953,  1093,  -953,  -953,  1058,  1060,   100,
    1062,  4751,  1064,  1733,  1071,   249,  1096,  -953,  -953,   705,
    1107,  -953,   815,  1339,    36,  -953,  1109,  -953,  1110,  -953,
    -953,   104,  -953,  -953,   104,  1066,  -953,  -953,   101,  4538,
    1499,  -953,  -953,  -953,  1114,  -953,  -953,  -953,   972,  1103,
     972,  1446,  1119,  1120,   560,    55,   596,  -953,  -953,  -953,
     181,  -953,  1116,   908,  1481,  1115,  2071,  2071,   401,   341,
    1061,  1061,   341,   341,  -953,  1994,  1126,  4538,  1097,    25,
    3060,  3451,  -953,  1131,  -953,  1134,  -953,   815,  -953,  -953,
    -953,  -953,  -953,  1135,  -953,  1085,   815,  1142,  -953,  3060,
     101,  -953,  1888,   616,  -953,  1144,  1148,  1143,   398,  -953,
    -953,  -953,  1145,  -953,  -953,  -953,    55,  -953,  -953,  1888,
    1481,  1151,   341,   341,  1149,  4538,  1159,  4538,  1161,  1162,
       7,  2490,  1163,  -953,  -953,  1165,  -953,  -953,    46,  -953,
    -953,  2071,   972,   181,  -953,  -953,  -953,    55,  1167,  -953,
    -953,  1168,  1116,   181,  -953,  -953,  -953,  1179,   815,  1180,
    4538,  4538,  4538,  1182,  -953,  1717,  -953,  4751,   520,  -953,
    -953,  1176,  1183,  -953,  -953,  -953,  1994,   815,  -953,   586,
     815,  1187,  1189,  1191,  4538,  -953,  1190,  -953,  1185,  -953,
    -953,  -953,   631,  -953,  -953,   560,  -953,   815,   815,   815,
    1194,   520,  1193,  -953,  -953,   560,  1195,  -953,  -953,  -953,
     815,  -953,  -953,  1200,  -953,  -953,  -953
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
     572,     0,     0,     0,     0,     0,    10,     4,   526,   390,
     398,   391,   392,   395,   396,   393,   394,   379,   397,   378,
     399,   572,   382,   400,   401,   402,   403,     0,   369,   370,
     371,   493,   494,   146,   488,   489,     0,   527,   528,     0,
       0,   538,     0,     0,     0,   367,   572,   374,   385,   377,
     387,   388,   492,     0,   572,   383,   536,     6,     0,     0,
     572,     1,    15,    64,    60,    61,     0,   261,    14,   257,
     572,     0,     0,    82,    83,   572,   572,     0,     0,   260,
     262,   263,     0,   264,   265,   270,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       9,    11,    18,    19,    20,    21,    22,    23,    24,    25,
     572,    26,    27,    28,    29,    30,    31,    32,     0,    33,
      34,    35,    36,    37,    38,    12,   113,   118,   115,   114,
      16,    13,   155,   156,   157,   158,   159,   160,     0,   275,
     572,   380,   525,     0,   148,   147,     0,     0,     0,     0,
       0,   381,     3,   373,   368,   572,     0,   404,     0,     0,
     538,   352,   351,   366,     0,   298,   281,   572,   305,   572,
     348,   342,   332,   295,   375,   389,   384,   544,     0,     0,
     534,     5,     8,     0,   276,   572,   278,    17,     0,   556,
     273,     0,   258,     0,     0,   563,     0,     0,   372,   572,
       0,     0,     0,     0,    78,     0,   572,   268,   272,   572,
     266,   269,   267,   274,   271,     0,     0,   190,   572,     0,
       0,    62,    63,     0,     0,    51,    49,    46,    47,   572,
       0,   572,     0,   572,   572,     0,   112,   572,   572,     0,
       0,     0,     0,     0,     0,     0,   332,     0,   572,     0,
     572,   558,   430,   431,   442,   443,   444,   445,   446,   447,
     448,   449,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   289,     0,   284,   572,   423,   372,     0,   422,   424,
     428,   425,   429,   286,   283,     0,     0,     0,   539,   537,
       0,   376,   572,   352,   351,     0,     0,   342,   383,     0,
     293,   409,   410,   291,     0,   406,   407,   408,   358,     0,
     422,   294,     0,   572,     0,     0,   307,   350,   324,     0,
     306,   349,   364,   365,   333,   296,   572,     0,   297,   572,
       0,     0,   345,   344,   302,   343,   324,   353,   543,   542,
     541,     0,     0,   277,   280,   530,   529,     0,   531,     0,
     555,   116,   259,   566,     0,    68,    45,     0,   572,   404,
      70,     0,     0,     0,    74,     0,     0,     0,    98,     0,
       0,   186,   119,   572,     0,   188,     0,     0,   103,     0,
       0,     0,   107,   299,   300,   301,    42,     0,   104,   106,
     532,     0,   533,    54,     0,    53,     0,     0,   179,   572,
     183,   492,   181,   170,     0,     0,     0,     0,   529,     0,
       0,     0,     0,     0,     0,   324,     0,     0,   332,   572,
     572,   412,   572,   572,   476,     0,   475,   384,   478,     0,
       0,     0,   440,   439,   468,   467,   441,   469,   470,   524,
       0,   285,   288,   471,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   557,   490,   491,   386,   535,     0,   352,   351,   342,
     383,     0,   332,     0,   362,   360,   345,   344,     0,   332,
     353,     0,     0,   405,   359,   572,   342,   383,     0,   325,
     572,     0,     0,   363,     0,   338,     0,     0,   356,     0,
       0,     0,   304,   347,     0,   303,   346,   354,     0,     0,
       0,   308,   540,     7,   572,     0,   171,   572,     0,     0,
     562,     0,     0,    69,    39,    77,     0,     0,     0,     0,
       0,     0,     0,   187,     0,     0,   572,   572,     0,     0,
     108,     0,   572,     0,     0,     0,     0,     0,   168,     0,
     180,   185,    58,     0,     0,     0,     0,    79,     0,     0,
       0,     0,     0,   150,     0,   383,     0,   502,   497,   498,
       0,   127,   572,   503,   572,   572,   163,   167,     0,   432,
       0,     0,   366,     0,   572,     0,   572,   465,   464,   462,
     463,     0,   461,   460,   456,   457,   455,   458,   459,   450,
     451,   452,   453,   454,     0,     0,   353,   336,   335,   334,
     354,     0,   315,     0,     0,     0,   324,   326,   353,     0,
       0,   329,     0,     0,   340,   339,   361,   357,     0,     0,
       0,     0,     0,     0,   309,   355,     0,     0,     0,   311,
     279,    66,    67,    65,     0,   567,   568,   571,   570,   564,
      44,    43,     0,    76,    73,    75,   561,    93,   560,     0,
      88,   572,   559,    92,     0,   570,     0,     0,    99,   572,
     228,   191,   192,     0,   257,     0,     0,    50,    48,   572,
      41,   105,     0,   549,   547,     0,    57,     0,     0,   110,
       0,   572,   572,   572,   572,     0,     0,   133,   132,   572,
     135,   572,   137,   131,   136,   141,     0,   149,   151,   572,
     572,   572,     0,   504,   500,   499,   126,     0,   123,   125,
     121,   128,   572,   129,   495,   477,   479,   481,   496,     0,
     161,   433,     0,     0,   366,   365,     0,     0,     0,     0,
       0,   287,     0,   572,   337,   292,   341,   327,     0,   317,
     331,   330,   316,   312,     0,     0,     0,     0,     0,   310,
       0,     0,     0,   117,     0,     0,   199,   219,     0,     0,
       0,     0,   262,     0,     0,   241,   242,   234,   243,   217,
     197,   239,   235,   233,   236,   237,   238,   240,   218,   214,
     215,   201,   209,   208,   212,   211,     0,     0,   202,   203,
     207,   213,   204,   205,   206,   216,     0,   275,   572,   506,
     507,     0,   509,     0,     0,     0,     0,    90,   572,     0,
     189,   258,     0,   572,   101,     0,   100,     0,     0,     0,
       0,   545,   572,     0,    52,     0,   257,     0,   172,   173,
     177,   176,   169,   174,   178,   175,     0,   184,     0,     0,
      81,   134,     0,   572,   142,     0,   413,   418,     0,   414,
     572,   404,   507,   572,   154,     0,     0,   572,   130,   572,
     486,   485,   487,     0,   483,     0,     0,   436,   435,   434,
       0,     0,   426,     0,   466,   282,   328,   314,   313,     0,
       0,     0,   318,     0,     0,   569,   565,     0,   194,   231,
     230,   232,     0,   229,     0,    40,   193,   379,   378,   572,
     382,     0,     0,     0,   377,   383,     0,   508,    84,   570,
      95,    89,   572,     0,     0,    97,     0,    71,     0,   109,
     550,   548,   554,   553,   552,     0,    55,    56,     0,   572,
       0,    59,    80,   122,     0,   144,   143,   140,   572,   419,
     572,     0,     0,     0,     0,     0,     0,   517,   501,   505,
       0,   480,   572,   572,     0,     0,   438,   437,   572,   319,
       0,     0,   323,   322,   200,     0,     0,   572,   380,     0,
       0,   572,   210,     0,    96,     0,    91,   572,    86,    72,
     102,   546,   551,     0,   120,     0,   572,     0,   417,     0,
     416,   152,   572,     0,   514,     0,   516,   518,     0,   510,
     511,   124,     0,   473,   482,   474,     0,   165,   164,   572,
       0,     0,   321,   320,     0,   572,     0,   572,     0,     0,
       0,     0,     0,    94,    85,     0,   111,   168,     0,   145,
     420,   421,   572,     0,   512,   513,   515,     0,     0,   522,
     523,     0,   572,     0,   162,   427,   195,     0,   572,     0,
     572,   572,   572,     0,   250,   572,    87,     0,     0,   415,
     153,   519,     0,   472,   484,   166,     0,   572,   221,     0,
     572,     0,     0,     0,   572,   220,     0,   138,     0,   520,
     196,   222,     0,   244,   246,     0,   227,   572,   572,   572,
       0,     0,     0,   247,   249,   404,     0,   225,   224,   223,
     572,   139,   521,     0,   245,   226,   248
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -953,  -953,  -353,  -953,  -953,  -953,  -953,    31,    62,     6,
      68,  -953,   726,  -953,    73,    79,  -953,  -953,  -953,    90,
    -953,   107,  -953,   109,  -953,  -953,   111,  -953,   113,  -529,
    -647,   117,  -953,   121,  -953,  -351,   707,   -89,   122,   123,
     126,   144,  -953,   556,  -823,  -663,  -953,  -953,  -953,  -952,
    -736,  -953,  -130,  -953,  -953,  -953,  -953,  -953,    51,  -953,
    -953,   232,    58,    59,  -953,  -953,   316,  -953,   710,   568,
     147,  -953,  -953,  -953,  -696,  -953,  -953,  -953,  -953,   575,
    -953,   579,   163,   580,  -953,  -953,  -953,  -461,  -953,  -953,
      27,   -48,  -953,   759,    13,   442,  -953,   686,   841,    26,
    -567,  -953,   -21,  1146,  -194,   -53,   932,    39,    44,  -953,
     -69,     8,   -37,   719,  -531,  1231,  -953,  -358,  -953,  -154,
    -953,  -953,  -953,  -909,  -953,   283,  -953,  1239,  -124,  -484,
    -953,  -953,   233,   861,  -953,  -953,  -953,   417,  -953,  -953,
    -953,  -225,    -3,   326,   725,  -397,  -571,   229,  -953,  -953,
     255,   -15,   353,  -157,  -953,   964,  -174,  -126,  -953,  -267,
    1075,  -953,   623,    -4,  -196,  -506,     0
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,     5,   100,   101,   102,   652,   775,   776,   777,
     778,   107,   393,   394,   779,   780,   693,   110,   111,   781,
     113,   782,   115,   783,   654,   203,   784,   118,   785,   660,
     529,   786,   367,   787,   377,   232,   388,   233,   788,   789,
     790,   791,   517,   126,   720,   570,   701,   127,   706,   855,
     947,   997,    42,   562,   128,   129,   130,   131,   792,   876,
     729,  1018,   793,   794,   691,   842,   397,   398,   399,   550,
     795,   136,   536,   373,   796,   975,  1076,   897,   797,   798,
     799,   800,   801,   802,   803,   804,  1078,  1091,   805,   912,
     806,   295,   184,   343,   185,   272,   273,   441,   274,   571,
     166,   382,   167,   316,   168,   169,   170,   245,    44,    45,
     275,   198,    47,    48,    49,    50,    51,   303,   304,   345,
     306,   307,   419,   857,   858,   948,  1040,   277,   310,   279,
     280,  1013,  1014,   425,   426,   575,   725,   726,   873,   963,
     874,    52,    53,   727,   573,   810,  1092,   864,   956,  1006,
    1007,   177,    54,   353,   391,    55,   180,    56,   685,   831,
     281,   282,   663,   194,   354,   649,   186
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
       6,   522,   197,   305,   230,   139,   141,   153,   248,    46,
      46,   105,   183,   401,    43,    58,   278,   739,   689,   816,
     534,   142,   666,   289,   231,   231,   574,   378,   703,   943,
     541,   347,   138,   875,   189,   647,   103,   355,   852,   998,
     656,  1000,   189,   361,   647,   410,   173,  1062,   365,   300,
     617,   189,   338,   340,   142,     8,   132,   617,     8,   350,
     927,    61,  -290,   133,   134,  1027,   350,   104,    46,   249,
     195,   201,   165,   106,   144,   195,   204,   390,   108,   657,
     350,   350,   658,   350,   109,   171,  1068,   412,   326,   154,
     811,   174,   854,   145,   560,   112,   239,   244,   819,   471,
     473,   906,   296,   478,     8,   327,   240,  -531,   301,   302,
     195,  -182,   114,   190,   116,   465,   117,   850,   119,   552,
     140,   351,   120,   849,  -290,   618,   121,   122,   123,   143,
     190,   124,   696,  1069,  1101,   250,  1063,  1011,   817,   863,
     283,    37,    38,   146,    37,    38,   191,   290,   276,   125,
     659,  1028,   135,   271,  1029,     8,   416,  1067,   368,   163,
     352,   369,   352,    46,   644,   148,   512,   173,   137,   173,
      39,   288,     8,  -182,    41,   722,   358,     8,   384,     8,
     965,   380,   716,   673,   736,   344,    59,   676,   349,   396,
      37,    38,   313,   325,   297,   328,   350,   291,     8,   142,
     314,   974,   322,   140,   960,   305,   178,   323,   359,   158,
     383,   471,   473,   478,    46,   329,   213,    46,   142,     8,
    1070,   829,   149,   717,   718,    60,   417,   719,   423,   179,
    1075,   385,   330,   389,   392,   313,   546,    46,   402,   296,
     147,    37,    38,   319,   830,    46,    46,   924,   421,   214,
     428,   400,   158,    28,    29,    30,   518,   140,    37,    38,
     296,   221,   374,    37,    38,    37,    38,    39,   519,   737,
     171,    41,   738,   984,   442,   914,   173,   936,   494,  1024,
     407,   497,   918,   150,    37,    38,   611,   612,   339,    39,
     315,     8,   982,    41,  -411,   930,   152,  -411,   542,   641,
      46,   189,   165,   222,   574,    37,    38,   920,   163,   430,
      39,   140,   231,   862,    41,   171,   278,   431,   231,   937,
     577,    46,   645,  -572,   401,  -256,   712,  -411,   155,   647,
    -572,   469,  1087,   315,    46,   933,   156,    46,   163,  1043,
     848,   642,   157,   181,   157,   158,   326,   607,     8,   687,
      46,   985,   486,   424,   140,   514,  1053,   182,   173,   611,
       8,   584,   493,   327,   159,  1111,   159,    31,    32,   140,
       8,   664,   187,     6,   564,   568,   569,    37,    38,   371,
    1090,   490,  -572,  -572,   521,   155,    34,    35,   188,   656,
     372,   189,  -572,   156,   292,   988,   407,   411,   491,   551,
     193,  1021,   158,    39,     8,   314,  -572,   160,   142,   853,
       8,   158,   917,   189,   161,   854,  1035,   162,   238,   563,
     142,   200,   163,   173,   199,  1038,   164,   428,   824,   202,
      46,   658,    31,    32,    37,    38,     8,   614,  1049,   825,
     218,   155,   620,   227,  1050,   189,    37,    38,   276,   156,
     555,    34,    35,   271,   205,   157,    37,    38,   158,   215,
      39,   407,   677,   604,   160,   678,   206,   572,   583,   231,
     574,   161,    39,   411,   162,   708,    41,   159,   709,   163,
     209,   319,    39,   164,   326,   608,    41,  1079,   326,   609,
      37,    38,     8,    46,   863,   315,    37,    38,    46,    31,
      32,   327,     8,   953,     8,   327,   661,   210,   326,   744,
     669,   220,   301,   302,   344,   223,    39,     6,    34,    35,
     160,   234,    37,    38,   707,   327,   211,   161,   235,   357,
     162,   212,   237,   694,   139,   163,     6,   139,    46,   164,
     105,   346,   389,   668,   950,   189,   346,   346,    39,   951,
     939,   284,    41,   346,    46,   363,   364,    46,   346,   688,
     945,   138,   400,   946,   675,   103,   490,   747,   895,   896,
     285,   315,   173,   376,   173,   728,   346,   379,    37,    38,
       1,     2,     3,   491,   173,   132,   442,   346,    37,    38,
      37,    38,   133,   134,   408,   286,   104,   346,   721,   299,
     723,   420,   106,   337,    39,   574,   822,   108,   160,   224,
     165,   312,   225,   109,    39,   226,   682,   341,    41,  1096,
     683,   322,   574,   171,   112,   405,   735,   164,   406,  1093,
     342,   827,  1094,   163,   207,   208,  1107,  1108,  1109,  1009,
    1095,   114,  1010,   116,   139,   117,   818,   119,   904,  1115,
     105,   120,   807,   490,   886,   121,   122,   123,   664,  1044,
     124,   812,  1045,   865,   991,   301,   302,   992,   862,   812,
     491,   138,   356,   139,  1103,   103,   248,  1104,   125,   105,
     566,   135,   527,   528,   851,  1105,   375,    46,   567,   568,
     569,   139,   551,     6,   812,   132,   867,   137,   153,   142,
     138,   173,   133,   134,   103,   809,   104,   305,   278,   859,
     812,   350,   106,   809,   401,   856,   381,   108,   846,    46,
     544,   545,   173,   109,   132,    28,    29,    30,   653,   807,
     189,   133,   134,   386,   112,   104,   928,   913,   809,   814,
     815,   106,   839,   173,   387,   572,   108,  1113,   868,   840,
     841,   114,   109,   116,   809,   117,   231,   119,   416,   439,
     916,   120,   440,   112,   461,   121,   122,   123,   443,   885,
     124,     8,   466,   322,   493,   462,   976,   913,   735,   463,
     114,   464,   116,   474,   117,   475,   119,   480,   125,   481,
     120,   135,   482,   922,   121,   122,   123,   807,   485,   124,
    1005,   926,     8,   250,   488,   944,   163,   137,   458,   459,
     460,   492,   952,   500,   968,   507,    46,   125,   812,   513,
     135,   515,   520,   142,   454,   455,   456,   457,   458,   459,
     460,   523,   283,   524,   239,   525,   137,     8,   526,   313,
     276,   531,   979,   250,   424,   271,   530,   331,   139,   456,
     457,   458,   459,   460,   105,   532,   158,    37,    38,   533,
     537,    46,   809,   957,    28,    29,    30,   173,    46,   728,
     538,   346,   539,   540,   313,   138,   807,   543,   547,   103,
     549,   346,   501,    39,   870,   871,   872,    41,    37,    38,
     548,   158,   423,   553,   978,   566,   684,   807,   346,   132,
     554,   557,   987,   567,   568,   569,   133,   134,   558,   142,
     104,   572,   559,   576,    39,   561,   106,   585,    41,   417,
     619,   108,   812,    37,    38,   332,   606,   109,   333,  1026,
    1002,   623,   624,  1032,   610,   626,   628,   315,   112,   402,
     629,  1106,   650,  1019,   630,   636,   637,    46,   859,    39,
     859,   305,   400,    41,   856,   114,   856,   116,   651,   117,
     667,   119,  1015,   728,   670,   120,   809,   672,   173,   121,
     122,   123,   315,   679,   124,   807,   680,  1057,   686,  1059,
     690,   710,   705,   711,   715,    46,   724,   812,   730,    46,
     714,   746,   125,   740,   165,   135,   812,   750,  1086,  1019,
     754,   764,   173,   157,     8,   755,   151,   171,   756,   757,
     172,   137,  1081,  1082,  1083,   760,   761,   176,  1030,   173,
     407,   762,   765,   808,   813,   159,   828,   833,   823,   566,
     832,   809,   834,    46,    71,    46,  1100,   567,   568,   569,
     809,   411,   859,   883,   869,   880,   572,   881,   856,   331,
     216,   219,  1015,   882,   899,   889,   890,   891,   812,     8,
     444,   445,   893,   572,     8,   812,   894,   898,    46,    46,
      46,   900,     8,   901,   902,   903,   807,   812,   905,  -198,
     812,   923,   246,   454,   455,   456,   457,   458,   459,   460,
      37,    38,    46,   317,   321,   925,   313,   812,   812,   812,
     929,   411,   809,   335,   504,   938,   942,   954,   958,   809,
     812,     8,   287,   158,   189,   217,    39,   346,   346,   298,
      41,   809,   955,   964,   809,   318,   318,   413,   324,   970,
     414,   971,     8,   977,  -253,   336,  -252,   983,  -255,   315,
     980,   809,   809,   809,   192,    37,    38,  -251,   357,   815,
      37,    38,   989,   990,   809,   996,   156,   999,    37,    38,
    1003,   246,  1020,  1004,  1012,   362,  1025,   228,   346,   155,
     993,    39,   236,  -254,  1033,    41,    39,  1034,  1036,   370,
      41,   931,   932,   934,    39,  1039,   158,  1046,    41,  1037,
    1047,  1048,  1055,  1051,   315,   172,  1056,    37,    38,   315,
    1058,  1060,  1061,   403,  1065,   409,   318,   318,  1066,   415,
     949,   418,   151,   246,   427,  1072,  1073,     8,    37,    38,
    1077,  1080,  1084,    39,  1088,   317,   321,   160,  1097,   335,
    1098,  1089,  1099,  1102,   242,  1110,   854,   243,  1114,     8,
     172,  1112,   163,  1116,    39,   646,   164,   838,   160,   681,
       8,   656,  1054,   189,   241,   995,   470,   472,   472,   692,
     847,   479,   156,   348,   503,   506,   843,   164,   348,   348,
     844,   845,   741,   640,   935,   348,   411,   487,   702,   489,
     348,   586,   175,  1042,   501,  1074,   961,   357,   578,  1016,
     657,   994,   713,   658,  1085,   156,   318,   318,   348,   826,
       0,   318,  1071,    37,    38,     0,     0,     0,   320,   348,
     395,     0,     0,   516,     0,   404,   348,   334,     0,   348,
       8,   409,     0,     0,     0,    37,    38,     0,     0,    39,
       0,     0,     0,   160,     0,     0,    37,    38,   535,     0,
     242,     0,     8,   243,     0,     0,     0,     0,   163,     0,
       0,    39,   164,   949,     0,    41,     0,   357,     0,     0,
     921,   659,    39,     0,     0,   156,   160,     0,     0,   472,
     472,   472,     0,   242,   315,   556,   243,   318,   318,   357,
     318,   163,   986,     0,     0,   164,   565,   156,     0,   320,
       0,   334,     0,     0,     0,     0,     0,   309,   311,   317,
     321,   335,     0,     0,     0,     0,    37,    38,   503,   506,
       8,     0,     0,   189,     0,     0,     0,     0,   335,     0,
     254,   255,   256,   257,   258,   259,   260,   261,    37,    38,
       0,     0,    39,   605,     0,     0,   160,     0,     0,   635,
       0,     0,     0,   242,   472,   472,   243,     0,     0,     8,
     616,   163,     0,     0,    39,   164,     0,     0,   160,     0,
       0,     0,     0,     0,     0,   242,     0,     0,   243,     0,
      67,   318,     0,   163,   318,     0,     0,   164,   502,   505,
       0,     0,     0,   511,     8,     0,   357,     0,     0,  1001,
       0,   246,     0,   836,   156,   246,    37,    38,     0,     0,
       0,   429,     8,   432,   433,   434,   435,   436,   437,   438,
       0,     0,   301,   302,     0,     0,     0,   472,   246,   318,
       0,   357,   318,   704,  1017,     0,    79,    80,    81,   156,
       0,    83,   773,    84,    85,    37,    38,     0,     0,   357,
       0,     0,   635,   483,     0,     0,     0,   156,   172,     8,
       0,     0,   189,     0,     0,     0,     0,     0,     0,   502,
     505,    39,   511,     0,     0,   160,   496,   837,   743,   499,
      37,    38,   242,     0,     0,   243,     0,     0,     0,     0,
     163,     0,   318,   318,   164,    67,     0,   318,    37,    38,
     643,     0,   318,   348,   648,     0,    39,   318,     0,     0,
     160,   655,   662,   665,     0,     0,     0,   242,   674,     0,
     243,     0,     0,     0,    39,   163,     0,     0,   160,   164,
     348,     8,   662,     0,     0,   242,     0,     0,   243,   695,
       0,   246,   615,   163,     0,    37,    38,   164,     0,     0,
       0,    79,    80,    81,     0,     0,    83,     0,    84,    85,
       0,   301,   302,   634,   835,     0,   639,     0,   422,     0,
     318,    39,     0,     0,     0,    41,   156,     0,     8,     0,
       0,     0,   405,     8,     0,   406,     0,     0,     0,     0,
     163,   246,     0,   587,   588,   589,   590,   591,   592,   593,
     594,   595,   596,   597,   598,   599,   600,   601,   602,   603,
       0,   615,     0,     0,   634,   313,     0,    37,    38,     0,
     155,     0,     0,   631,     0,     0,     0,     0,   318,   318,
       0,   613,   158,     0,   318,     0,     0,   158,     0,     0,
     622,     0,     0,    39,     0,     0,     8,   160,     8,     0,
       0,     0,   662,     0,   242,     0,     0,   243,     0,   821,
       0,   662,   163,     0,    37,    38,   164,   860,     8,    37,
      38,     0,     0,     0,   748,   749,    28,    29,    30,   752,
     915,   861,     0,   981,   753,   411,     0,     0,   246,   759,
      39,   156,     0,   508,    41,    39,   246,     0,     0,   160,
       0,   632,     0,     0,   633,   313,   476,   566,     0,   477,
       0,     0,     0,   315,     0,   567,   568,   569,   164,     0,
       0,     8,   158,     0,   189,     0,     0,     0,   731,   593,
     596,   601,    37,    38,    37,    38,     0,     0,     0,     0,
       0,     8,     0,     0,     0,     0,     0,   962,     0,   348,
     348,     0,   748,     0,    37,    38,     0,     0,    39,   360,
      39,     0,   160,   318,    41,     0,     0,   318,   318,   242,
       0,   509,   243,     0,   510,     0,     0,   163,   411,     0,
      39,   164,     0,   315,    41,     8,   631,   246,     0,     0,
       0,   476,     0,     0,   477,     0,   662,   246,   919,     0,
     348,     0,     0,   315,     0,     0,     0,    37,    38,     0,
     887,   888,     0,     0,   246,     0,   892,     0,     0,     0,
       0,   821,   411,   301,   302,   246,     0,    37,    38,  1008,
     504,     0,     0,     0,     0,     0,     0,     0,   246,     0,
       0,     0,   172,     0,   318,   318,     0,    28,    29,    30,
       0,     0,   157,    39,     0,   565,     0,    41,     0,     0,
       0,   866,     0,     0,   632,     0,     0,   633,     0,     0,
       0,    37,    38,     0,   159,     0,   315,     0,   566,     0,
       0,   877,   878,   436,     0,   879,   567,   568,   569,     0,
    1052,   884,     0,     0,   246,     0,     0,    39,     0,     0,
       0,    41,     0,     0,     0,   766,     0,  -572,    63,   662,
       0,     0,    64,    65,    66,     0,     0,     0,     0,     0,
     315,  1008,     0,     0,     0,    67,  -572,  -572,  -572,  -572,
    -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,
    -572,  -572,  -572,  -572,     0,   969,     0,   767,    69,   972,
     973,  -572,     0,  -572,  -572,  -572,  -572,  -572,     0,     0,
       0,     0,     0,     0,     0,     0,    71,    72,    73,    74,
     768,    76,    77,    78,  -572,  -572,  -572,   769,   770,   771,
       0,    79,   772,    81,     0,    82,    83,   773,    84,    85,
    -572,  -572,     0,  -572,  -572,    86,     0,     0,     0,    90,
     189,    92,    93,    94,    95,    96,    97,   254,   255,   256,
     257,   258,   259,   260,   261,     0,     0,    98,     0,  -572,
       0,     0,    99,  -572,  -572,     0,  1022,  1023,     0,   966,
     967,     0,     0,     0,     0,     8,     0,     0,   189,   251,
       0,     8,   774,   252,   253,   254,   255,   256,   257,   258,
     259,   260,   261,     0,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,   262,     0,     0,     0,     0,     0,   411,     0,
      27,    28,    29,    30,    31,    32,   638,   263,   444,   445,
     446,   447,     0,   448,     0,     0,     0,     0,     0,     0,
       0,     0,    33,    34,    35,     0,   449,   450,   451,   452,
     453,   454,   455,   456,   457,   458,   459,   460,    36,     0,
       0,    37,    38,     8,     0,     0,     0,    37,    38,  1031,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    39,  1041,     0,
      40,    41,     0,    39,     0,     0,     0,    41,   264,     0,
     411,   265,     0,     0,   266,   267,   268,     0,   758,     8,
     269,   270,   189,   251,     0,   579,   315,   252,   253,   254,
     255,   256,   257,   258,   259,   260,   261,     0,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,   262,     0,     0,    37,
      38,     0,     0,     0,     0,    28,    29,    30,    31,    32,
       0,   263,     0,     0,   308,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    39,    33,    34,    35,    41,
       0,   444,   445,   446,   447,     0,   448,     0,   444,   445,
     446,   447,    36,     0,     0,    37,    38,     0,   315,   449,
     580,   451,   452,   581,   454,   455,   456,   457,   582,   459,
     460,   454,   455,   456,   457,   458,   459,   460,     0,     0,
       0,    39,     0,     0,     0,    41,     0,     0,     0,     0,
       0,     0,   264,     0,     0,   265,     0,     0,   266,   267,
     268,     0,     0,     8,   269,   270,   189,   251,   959,     0,
       0,   252,   253,   254,   255,   256,   257,   258,   259,   260,
     261,     0,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
     262,     0,     0,     0,     0,     0,     0,     0,     0,    28,
      29,    30,    31,    32,     0,   263,     0,     0,   495,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      33,    34,    35,     0,   444,   445,   446,   447,     0,   448,
       0,     0,   444,   445,   446,   447,    36,     0,     0,    37,
      38,     0,   449,   450,   451,   452,   453,   454,   455,   456,
     457,   458,   459,   460,   453,   454,   455,   456,   457,   458,
     459,   460,     0,     0,     0,    39,     0,     0,     0,    41,
       0,     0,     0,     0,     0,     0,   264,     0,     0,   265,
       0,     0,   266,   267,   268,     0,     0,     8,   269,   270,
     189,   251,     0,  1064,     0,   252,   253,   254,   255,   256,
     257,   258,   259,   260,   261,     0,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,   262,     0,     0,     0,     0,     0,
       0,     0,     0,    28,    29,    30,    31,    32,     0,   263,
       0,     0,   498,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    33,    34,    35,   444,   445,   446,
     447,     0,   448,     0,     0,     0,   444,   445,   446,   447,
      36,     0,     0,    37,    38,   449,   450,   451,   452,   453,
     454,   455,   456,   457,   458,   459,   460,   452,   453,   454,
     455,   456,   457,   458,   459,   460,     0,     0,     0,    39,
       0,     0,     0,    41,     0,     0,     0,     0,     0,     0,
     264,     0,     0,   265,     0,     0,   266,   267,   268,     0,
       0,     8,   269,   270,   189,   251,     0,     0,     0,   252,
     253,   254,   255,   256,   257,   258,   259,   260,   261,     0,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,   262,     0,
     742,     0,     0,     0,     0,     0,     0,    28,    29,    30,
      31,    32,     0,   263,     0,     0,   621,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    33,    34,
      35,   444,   445,   446,   447,     0,   448,     0,     0,     0,
       0,     0,     0,     0,    36,     0,     0,    37,    38,   449,
     450,   451,   452,   453,   454,   455,   456,   457,   458,   459,
     460,     0,     0,     0,     0,     9,    10,    11,    12,    13,
      14,    15,    16,    39,    18,     0,    20,    41,     0,    23,
      24,    25,    26,     0,   264,     0,     0,   265,     0,     0,
     266,   267,   268,     0,     0,     8,   269,   270,   189,   251,
       0,     0,     0,   252,   253,   254,   255,   256,   257,   258,
     259,   260,   261,     0,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,   262,   732,     0,     0,     0,     0,     0,     0,
       0,    28,    29,    30,    31,    32,     0,   263,   444,   445,
     446,   447,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    33,    34,    35,     0,   449,   450,   451,   452,
     453,   454,   455,   456,   457,   458,   459,   460,    36,     0,
       0,    37,    38,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    39,     0,     0,
       0,    41,     0,     0,     0,     0,     0,     0,   264,     0,
       0,   265,     0,     0,   266,   267,   268,     0,     0,     8,
     269,   270,   189,   251,     0,     0,     0,   252,   253,   254,
     255,   256,   257,   258,   259,   260,   261,     0,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,   262,   733,     0,     0,
       0,     0,     0,     0,     0,    28,    29,    30,    31,    32,
       0,   263,   444,   445,   446,   447,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    33,    34,    35,     0,
       0,   450,   451,   452,   453,   454,   455,   456,   457,   458,
     459,   460,    36,     0,     0,    37,    38,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    39,     0,     0,     0,    41,     0,     0,     0,     0,
       0,     0,   264,     0,     0,   265,     0,     0,   266,   267,
     268,     0,     0,     8,   269,   270,   189,   251,     0,     0,
       0,   252,   253,   254,   255,   256,   257,   258,   259,   260,
     261,     0,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
     262,     0,     0,     0,     0,     0,     0,     0,     0,    28,
      29,    30,    31,    32,     0,   263,   444,   445,   446,   447,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      33,    34,    35,     0,     0,     0,   451,   452,   453,   454,
     455,   456,   457,   458,   459,   460,    36,     0,     0,    37,
      38,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    39,     0,     0,     0,    41,
       0,     0,     0,     0,     0,     0,   264,     0,     0,   265,
       0,     0,   266,   267,   268,     0,     0,     8,   269,   270,
     189,   251,     0,     0,     0,   252,   253,   254,   255,   256,
     257,   258,   259,   260,   261,     0,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,   262,     0,     0,     0,     0,     0,
       0,     0,     0,    28,    29,    30,    31,    32,     0,   263,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    33,    34,    35,     0,     0,     8,
       0,     8,     0,     0,     0,     0,     0,     0,     0,     0,
      36,     0,     0,    37,    38,     0,     0,     0,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,   292,     0,   292,    39,
       0,     0,     0,    41,    27,    28,    29,    30,    31,    32,
       0,     0,     0,   158,     0,   158,   266,   267,   734,     0,
       0,     0,   269,   270,     0,     0,    33,    34,    35,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    36,     0,     0,    37,    38,    37,    38,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    39,     8,    39,    40,    41,     0,    41,     0,     0,
       0,     0,   293,     0,   476,   294,     0,   477,     0,     0,
     163,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,   292,
       0,     0,     0,     0,     0,     0,     0,    27,    28,    29,
      30,    31,    32,     0,     0,     0,   158,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    33,
      34,    35,     0,     0,     8,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    36,     0,     0,    37,    38,
       0,     0,     0,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,     0,     0,     0,    39,     0,     0,    40,    41,    27,
      28,    29,    30,    31,    32,   467,     0,     0,   468,     0,
       0,     0,     0,   163,     0,     0,     0,     0,     0,     0,
       0,    33,    34,    35,     8,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    36,     0,     0,
      37,    38,     0,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,     0,     0,     0,     0,     0,    39,     0,     0,    40,
      41,     0,     0,    31,    32,     0,     0,   405,     0,     0,
     406,     0,     0,     0,     0,   163,     0,     0,     0,     0,
       0,    33,    34,    35,    -2,    62,     0,  -572,    63,     0,
       0,     0,    64,    65,    66,     0,     0,    36,     0,     0,
      37,    38,     0,     0,     0,    67,  -572,  -572,  -572,  -572,
    -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,
    -572,  -572,  -572,  -572,     0,     0,    39,    68,    69,     0,
      41,     0,     0,  -572,  -572,  -572,  -572,  -572,     0,     0,
      70,     0,     0,     0,     0,   163,    71,    72,    73,    74,
      75,    76,    77,    78,  -572,  -572,  -572,     0,     0,     0,
       0,    79,    80,    81,     0,    82,    83,     0,    84,    85,
    -572,  -572,     0,  -572,  -572,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    62,     0,  -572,
      63,     0,     0,     0,    64,    65,    66,    98,     0,  -572,
       0,     0,    99,  -572,     0,     0,     0,    67,  -572,  -572,
    -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,
    -572,  -572,  -572,  -572,  -572,  -572,     0,     0,     0,    68,
      69,     0,     0,   671,     0,  -572,  -572,  -572,  -572,  -572,
       0,     0,    70,     0,     0,     0,     0,     0,    71,    72,
      73,    74,    75,    76,    77,    78,  -572,  -572,  -572,     0,
       0,     0,     0,    79,    80,    81,     0,    82,    83,     0,
      84,    85,  -572,  -572,     0,  -572,  -572,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    62,
       0,  -572,    63,     0,     0,     0,    64,    65,    66,    98,
       0,  -572,     0,     0,    99,  -572,     0,     0,     0,    67,
    -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,
    -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,     0,     0,
       0,    68,    69,     0,     0,   763,     0,  -572,  -572,  -572,
    -572,  -572,     0,     0,    70,     0,     0,     0,     0,     0,
      71,    72,    73,    74,    75,    76,    77,    78,  -572,  -572,
    -572,     0,     0,     0,     0,    79,    80,    81,     0,    82,
      83,     0,    84,    85,  -572,  -572,     0,  -572,  -572,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    62,     0,  -572,    63,     0,     0,     0,    64,    65,
      66,    98,     0,  -572,     0,     0,    99,  -572,     0,     0,
       0,    67,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,
    -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,
       0,     0,     0,    68,    69,     0,     0,   820,     0,  -572,
    -572,  -572,  -572,  -572,     0,     0,    70,     0,     0,     0,
       0,     0,    71,    72,    73,    74,    75,    76,    77,    78,
    -572,  -572,  -572,     0,     0,     0,     0,    79,    80,    81,
       0,    82,    83,     0,    84,    85,  -572,  -572,     0,  -572,
    -572,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    62,     0,  -572,    63,     0,     0,     0,
      64,    65,    66,    98,     0,  -572,     0,     0,    99,  -572,
       0,     0,     0,    67,  -572,  -572,  -572,  -572,  -572,  -572,
    -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,  -572,
    -572,  -572,     0,     0,     0,    68,    69,     0,     0,     0,
       0,  -572,  -572,  -572,  -572,  -572,     0,     0,    70,     0,
       0,     0,   941,     0,    71,    72,    73,    74,    75,    76,
      77,    78,  -572,  -572,  -572,     0,     0,     0,     0,    79,
      80,    81,     0,    82,    83,     0,    84,    85,  -572,  -572,
       0,  -572,  -572,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    97,     7,     0,     8,     0,     0,
       0,     0,     0,     0,     0,    98,     0,  -572,     0,     0,
      99,  -572,     0,     0,     0,     0,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,     0,     0,     0,     0,     0,     0,
       0,     0,    27,    28,    29,    30,    31,    32,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    33,    34,    35,    57,     0,     8,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      36,     0,     0,    37,    38,     0,     0,     0,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,     0,     0,     0,    39,
       0,     0,    40,    41,    27,    28,    29,    30,    31,    32,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    33,    34,    35,   196,
       0,     8,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    36,     0,     0,    37,    38,     0,     0,     0,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,     0,     0,
       0,    39,     0,     0,    40,    41,     0,    28,    29,    30,
      31,    32,     0,     0,     0,     0,     0,     0,     0,     0,
     484,     0,     0,     0,     0,     0,     0,     0,    33,    34,
      35,     0,     8,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    36,     0,     0,    37,    38,     0,
       0,     9,    10,    11,    12,    13,    14,    15,    16,   907,
      18,   908,    20,   909,   910,    23,    24,    25,    26,   444,
     445,   446,   447,    39,   448,     0,     0,    41,    28,    29,
      30,    31,    32,     0,     0,     0,     0,   449,   450,   451,
     452,   453,   454,   455,   456,   457,   458,   459,   460,    33,
      34,    35,     0,     0,     8,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    36,   247,     0,    37,    38,
       0,     0,     0,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,     0,     0,     0,    39,     0,     0,     0,    41,   911,
      28,    29,    30,    31,    32,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    33,    34,    35,     0,     0,     0,     0,     0,     8,
       0,     0,     0,     0,     0,     0,     0,    36,   940,   366,
      37,    38,     0,     0,     0,     0,     0,     0,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    39,     0,     0,     0,
      41,   911,     0,     0,    27,    28,    29,    30,    31,    32,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    33,    34,    35,     0,
       0,     8,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    36,     0,     0,    37,    38,     0,     0,     0,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,     0,     0,
       0,    39,     0,     0,    40,    41,    27,    28,    29,    30,
      31,    32,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    33,    34,
      35,     0,     8,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    36,     0,     0,    37,    38,     0,
       0,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,   229,
       0,     0,     0,    39,     0,     0,    40,    41,    28,    29,
      30,    31,    32,     0,     0,     0,     0,     0,     0,     0,
       0,   625,     0,     0,     0,     0,     0,     0,     0,    33,
      34,    35,     0,     8,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    36,     0,     0,    37,    38,
       0,     0,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
     444,   445,   446,   447,    39,   448,     0,     0,    41,    28,
      29,    30,    31,    32,     0,     0,     0,     0,   449,   450,
     451,   452,   453,   454,   455,   456,   457,   458,   459,   460,
      33,    34,    35,     0,     8,     0,     0,     0,   627,     0,
       0,     0,     0,     0,     0,     0,    36,   247,     0,    37,
      38,     0,     0,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,     0,     0,     0,     0,    39,     0,     0,     0,    41,
      28,    29,    30,    31,    32,     0,     0,   444,   445,   446,
     447,     0,   448,     0,     0,     0,     0,     0,     0,     0,
       0,    33,    34,    35,     8,   449,   450,   451,   452,   453,
     454,   455,   456,   457,   458,   459,   460,    36,     0,     0,
      37,    38,     0,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,     0,     0,     0,     0,     0,    39,     0,     0,     0,
      41,     0,     0,    31,    32,     0,     0,     0,     0,     0,
       0,     0,     0,   745,     0,     0,     0,     0,     0,     0,
       0,    33,    34,    35,     8,   751,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    36,     0,     0,
      37,    38,     0,     9,    10,    11,    12,    13,    14,    15,
      16,   697,    18,   698,    20,   699,   700,    23,    24,    25,
      26,     0,   444,   445,   446,   447,    39,   448,     0,     0,
      41,     0,     0,     0,   444,   445,   446,   447,     0,   448,
     449,   450,   451,   452,   453,   454,   455,   456,   457,   458,
     459,   460,   449,   450,   451,   452,   453,   454,   455,   456,
     457,   458,   459,   460,     0,     0,     0,    36,     0,     0,
      37,    38,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    39,     0,     0,     0,
      41
};

static const yytype_int16 yycheck[] =
{
       0,   359,    71,   157,    93,     5,    21,    44,   138,     1,
       2,     5,    60,   238,     1,     2,   140,   584,   547,   666,
     373,    21,   528,   149,    93,    94,   423,   223,   559,   852,
     381,   188,     5,   729,     6,   519,     5,   194,   701,   948,
       4,   950,     6,   200,   528,   241,    46,    40,   205,    46,
      41,     6,   178,   179,    54,     3,     5,    41,     3,     6,
      43,     0,    42,     5,     5,    40,     6,     5,    60,   138,
      70,    75,    46,     5,    52,    75,    76,   234,     5,    43,
       6,     6,    46,     6,     5,    46,  1038,   244,    40,    45,
     661,    47,    46,    71,    54,     5,    99,   118,   669,   293,
     294,   797,   155,   297,     3,    57,   110,    54,   105,   106,
     110,    42,     5,    85,     5,   289,     5,    43,     5,    59,
     103,    46,     5,   694,   104,   116,     5,     5,     5,    48,
      85,     5,   116,  1042,  1086,   138,   129,   960,   667,   710,
     140,    89,    90,    48,    89,    90,   118,   150,   140,     5,
     114,   126,     5,   140,   129,     3,   116,   111,   206,   134,
      85,   209,    85,   155,   517,    40,   340,   167,     5,   169,
     115,   119,     3,   104,   119,   572,   197,     3,   231,     3,
     876,   229,     1,   536,    41,   185,     1,   538,   191,   237,
      89,    90,    40,   167,   155,   169,     6,   153,     3,   199,
      48,   897,   163,   103,   867,   359,   117,   163,    54,    57,
     231,   405,   406,   407,   206,    40,    44,   209,   218,     3,
    1043,   117,   140,    42,    43,    40,   247,    46,   249,   140,
    1053,   231,    57,   233,   234,    40,    46,   229,   238,   292,
      48,    89,    90,    48,   140,   237,   238,   818,   248,    77,
     250,   238,    57,    49,    50,    51,    42,   103,    89,    90,
     313,     4,    54,    89,    90,    89,    90,   115,    54,   126,
     231,   119,   129,   920,   274,   806,   276,     4,   326,   975,
     241,   329,   811,   103,    89,    90,   480,   481,   119,   115,
     138,     3,    43,   119,    43,   119,    43,    46,   387,     4,
     292,     6,   276,    46,   701,    89,    90,   813,   134,    40,
     115,   103,   381,   710,   119,   276,   440,    48,   387,    46,
      43,   313,   518,    46,   549,    76,    40,    76,    40,   813,
      42,   292,  1068,   138,   326,   119,    48,   329,   134,  1002,
     693,    46,    54,    43,    54,    57,    40,    41,     3,   545,
     342,   922,   313,    76,   103,   342,  1019,    43,   358,   553,
       3,   430,   323,    57,    76,  1101,    76,    52,    53,   103,
       3,   528,   120,   373,   422,    89,    90,    89,    90,    43,
    1076,    40,   116,   117,   358,    40,    71,    72,    40,     4,
      54,     6,   104,    48,    40,   924,   357,    40,    57,   399,
      40,   968,    57,   115,     3,    48,   140,   119,   408,    40,
       3,    57,   809,     6,   126,    46,   987,   129,   103,   419,
     420,    40,   134,   423,    71,   996,   138,   427,    43,    40,
     422,    46,    52,    53,    89,    90,     3,   485,    40,    54,
      87,    40,   490,     4,    46,     6,    89,    90,   440,    48,
     411,    71,    72,   440,    40,    54,    89,    90,    57,    92,
     115,   422,    43,   466,   119,    46,    40,   423,   429,   538,
     867,   126,   115,    40,   129,    43,   119,    76,    46,   134,
      40,    48,   115,   138,    40,    41,   119,  1058,    40,    41,
      89,    90,     3,   485,  1065,   138,    89,    90,   490,    52,
      53,    57,     3,   861,     3,    57,   527,    84,    40,    41,
     531,    40,   105,   106,   514,    40,   115,   517,    71,    72,
     119,    40,    89,    90,   561,    57,    77,   126,    40,    40,
     129,    82,    40,   554,   534,   134,   536,   537,   530,   138,
     534,   188,   542,   530,    42,     6,   193,   194,   115,    47,
     103,    48,   119,   200,   546,   202,   203,   549,   205,   546,
      40,   534,   549,    43,   537,   534,    40,    41,   764,   765,
      48,   138,   572,   220,   574,   575,   223,   224,    89,    90,
     121,   122,   123,    57,   584,   534,   586,   234,    89,    90,
      89,    90,   534,   534,   241,    48,   534,   244,   572,    48,
     574,   248,   534,   116,   115,  1002,   675,   534,   119,    40,
     584,     3,    43,   534,   115,    46,   115,    41,   119,  1080,
     119,   582,  1019,   584,   534,   126,   582,   138,   129,    43,
      42,   679,    46,   134,    84,    85,  1097,  1098,  1099,    43,
      54,   534,    46,   534,   644,   534,   667,   534,   774,  1110,
     644,   534,   652,    40,    41,   534,   534,   534,   815,    43,
     534,   661,    46,   711,   931,   105,   106,   934,  1065,   669,
      57,   644,    43,   673,    43,   644,   806,    46,   534,   673,
      80,   534,    41,    42,   699,    54,    46,   679,    88,    89,
      90,   691,   692,   693,   694,   644,   717,   534,   735,   699,
     673,   701,   644,   644,   673,   661,   644,   861,   832,   709,
     710,     6,   644,   669,   939,   709,    46,   644,   691,   711,
      41,    42,   722,   644,   673,    49,    50,    51,     4,   729,
       6,   673,   673,    43,   644,   673,   825,   806,   694,    41,
      42,   673,   691,   743,    42,   701,   673,  1105,   722,   691,
     691,   644,   673,   644,   710,   644,   825,   644,   116,   104,
     808,   644,    42,   673,     7,   644,   644,   644,    40,   743,
     644,     3,   104,   734,   735,    48,   902,   846,   734,    48,
     673,    41,   673,    41,   673,    41,   673,   116,   644,    48,
     673,   644,    57,   814,   673,   673,   673,   797,    40,   673,
     954,   822,     3,   806,    48,   853,   134,   644,   134,   135,
     136,    48,   860,    48,   883,   116,   808,   673,   818,    43,
     673,    41,    41,   823,   130,   131,   132,   133,   134,   135,
     136,    41,   832,    46,   837,    41,   673,     3,    54,    40,
     832,    41,   911,   846,    76,   832,    42,    48,   848,   132,
     133,   134,   135,   136,   848,    41,    57,    89,    90,    43,
      41,   853,   818,   863,    49,    50,    51,   867,   860,   869,
      41,   518,    41,    41,    40,   848,   876,    41,    41,   848,
      42,   528,    48,   115,    73,    74,    75,   119,    89,    90,
     104,    57,   913,   116,   909,    80,   543,   897,   545,   848,
      41,    43,   923,    88,    89,    90,   848,   848,   116,   909,
     848,   867,   111,    46,   115,    76,   848,    48,   119,   940,
      48,   848,   922,    89,    90,   126,   116,   848,   129,   977,
     951,    48,    41,   981,   116,    41,    48,   138,   848,   939,
      48,  1095,    43,   964,    48,    48,    48,   939,   948,   115,
     950,  1105,   939,   119,   948,   848,   950,   848,    43,   848,
      41,   848,   962,   963,    46,   848,   922,    43,   968,   848,
     848,   848,   138,    40,   848,   975,    47,  1025,    43,  1027,
      91,    41,    57,    40,    89,   977,    78,   987,    46,   981,
      90,    41,   848,    48,   968,   848,   996,    41,  1067,  1020,
      48,    42,  1002,    54,     3,    48,    42,   968,    48,    48,
      46,   848,  1060,  1061,  1062,    48,    48,    53,   979,  1019,
     981,    48,    42,    40,    54,    76,   140,    41,   675,    80,
     103,   987,    47,  1025,    62,  1027,  1084,    88,    89,    90,
     996,    40,  1042,    40,    42,    41,  1002,    41,  1042,    48,
      86,    87,  1052,    41,    76,    48,    48,    48,  1058,     3,
     107,   108,    48,  1019,     3,  1065,    48,    46,  1060,  1061,
    1062,    76,     3,    76,   138,    40,  1076,  1077,    47,    47,
    1080,    41,   118,   130,   131,   132,   133,   134,   135,   136,
      89,    90,  1084,   161,   162,    43,    40,  1097,  1098,  1099,
      41,    40,  1058,   171,    48,    54,    43,    54,    41,  1065,
    1110,     3,   148,    57,     6,    46,   115,   764,   765,   155,
     119,  1077,    76,    47,  1080,   161,   162,   126,   164,    48,
     129,    48,     3,    40,    76,   171,    76,    41,    76,   138,
      76,  1097,  1098,  1099,    69,    89,    90,    76,    40,    42,
      89,    90,    43,    43,  1110,    41,    48,    54,    89,    90,
      41,   197,    47,    43,    48,   201,    40,    92,   815,    40,
     104,   115,    97,    76,    43,   119,   115,    43,    43,   215,
     119,   828,   829,   830,   115,    43,    57,    43,   119,   104,
      42,    48,    41,    48,   138,   231,    47,    89,    90,   138,
      41,    40,    40,   239,    41,   241,   242,   243,    43,   245,
     857,   247,   248,   249,   250,    48,    48,     3,    89,    90,
      41,    41,    40,   115,    48,   293,   294,   119,    41,   297,
      41,    48,    41,    48,   126,    41,    46,   129,    43,     3,
     276,    48,   134,    43,   115,   519,   138,   691,   119,   542,
       3,     4,  1020,     6,    40,   939,   292,   293,   294,   549,
     692,   297,    48,   188,   332,   333,   691,   138,   193,   194,
     691,   691,   586,   514,   832,   200,    40,   313,   559,   315,
     205,   440,    51,  1000,    48,  1052,   869,    40,   427,   963,
      43,   938,   567,    46,  1065,    48,   332,   333,   223,   676,
      -1,   337,  1047,    89,    90,    -1,    -1,    -1,   162,   234,
     235,    -1,    -1,   349,    -1,   240,   241,   171,    -1,   244,
       3,   357,    -1,    -1,    -1,    89,    90,    -1,    -1,   115,
      -1,    -1,    -1,   119,    -1,    -1,    89,    90,   374,    -1,
     126,    -1,     3,   129,    -1,    -1,    -1,    -1,   134,    -1,
      -1,   115,   138,  1000,    -1,   119,    -1,    40,    -1,    -1,
      43,   114,   115,    -1,    -1,    48,   119,    -1,    -1,   405,
     406,   407,    -1,   126,   138,   411,   129,   413,   414,    40,
     416,   134,    43,    -1,    -1,   138,   422,    48,    -1,   243,
      -1,   245,    -1,    -1,    -1,    -1,    -1,   158,   159,   467,
     468,   469,    -1,    -1,    -1,    -1,    89,    90,   476,   477,
       3,    -1,    -1,     6,    -1,    -1,    -1,    -1,   486,    -1,
      13,    14,    15,    16,    17,    18,    19,    20,    89,    90,
      -1,    -1,   115,   469,    -1,    -1,   119,    -1,    -1,   507,
      -1,    -1,    -1,   126,   480,   481,   129,    -1,    -1,     3,
     486,   134,    -1,    -1,   115,   138,    -1,    -1,   119,    -1,
      -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,   129,    -1,
      21,   507,    -1,   134,   510,    -1,    -1,   138,   332,   333,
      -1,    -1,    -1,   337,     3,    -1,    40,    -1,    -1,    43,
      -1,   527,    -1,    44,    48,   531,    89,    90,    -1,    -1,
      -1,   262,     3,   264,   265,   266,   267,   268,   269,   270,
      -1,    -1,   105,   106,    -1,    -1,    -1,   553,   554,   555,
      -1,    40,   558,   559,    43,    -1,    77,    78,    79,    48,
      -1,    82,    83,    84,    85,    89,    90,    -1,    -1,    40,
      -1,    -1,   610,   304,    -1,    -1,    -1,    48,   584,     3,
      -1,    -1,     6,    -1,    -1,    -1,    -1,    -1,    -1,   413,
     414,   115,   416,    -1,    -1,   119,   327,   118,   604,   330,
      89,    90,   126,    -1,    -1,   129,    -1,    -1,    -1,    -1,
     134,    -1,   618,   619,   138,    21,    -1,   623,    89,    90,
     515,    -1,   628,   518,   519,    -1,   115,   633,    -1,    -1,
     119,   526,   527,   528,    -1,    -1,    -1,   126,    44,    -1,
     129,    -1,    -1,    -1,   115,   134,    -1,    -1,   119,   138,
     545,     3,   547,    -1,    -1,   126,    -1,    -1,   129,   554,
      -1,   667,   486,   134,    -1,    89,    90,   138,    -1,    -1,
      -1,    77,    78,    79,    -1,    -1,    82,    -1,    84,    85,
      -1,   105,   106,   507,   690,    -1,   510,    -1,    40,    -1,
     696,   115,    -1,    -1,    -1,   119,    48,    -1,     3,    -1,
      -1,    -1,   126,     3,    -1,   129,    -1,    -1,    -1,    -1,
     134,   717,    -1,   444,   445,   446,   447,   448,   449,   450,
     451,   452,   453,   454,   455,   456,   457,   458,   459,   460,
      -1,   555,    -1,    -1,   558,    40,    -1,    89,    90,    -1,
      40,    -1,    -1,    48,    -1,    -1,    -1,    -1,   754,   755,
      -1,   482,    57,    -1,   760,    -1,    -1,    57,    -1,    -1,
     491,    -1,    -1,   115,    -1,    -1,     3,   119,     3,    -1,
      -1,    -1,   667,    -1,   126,    -1,    -1,   129,    -1,   674,
      -1,   676,   134,    -1,    89,    90,   138,    40,     3,    89,
      90,    -1,    -1,    -1,   618,   619,    49,    50,    51,   623,
     806,    54,    -1,    40,   628,    40,    -1,    -1,   814,   633,
     115,    48,    -1,    48,   119,   115,   822,    -1,    -1,   119,
      -1,   126,    -1,    -1,   129,    40,   126,    80,    -1,   129,
      -1,    -1,    -1,   138,    -1,    88,    89,    90,   138,    -1,
      -1,     3,    57,    -1,     6,    -1,    -1,    -1,   579,   580,
     581,   582,    89,    90,    89,    90,    -1,    -1,    -1,    -1,
      -1,     3,    -1,    -1,    -1,    -1,    -1,   873,    -1,   764,
     765,    -1,   696,    -1,    89,    90,    -1,    -1,   115,    41,
     115,    -1,   119,   889,   119,    -1,    -1,   893,   894,   126,
      -1,   126,   129,    -1,   129,    -1,    -1,   134,    40,    -1,
     115,   138,    -1,   138,   119,     3,    48,   913,    -1,    -1,
      -1,   126,    -1,    -1,   129,    -1,   811,   923,   813,    -1,
     815,    -1,    -1,   138,    -1,    -1,    -1,    89,    90,    -1,
     754,   755,    -1,    -1,   940,    -1,   760,    -1,    -1,    -1,
      -1,   836,    40,   105,   106,   951,    -1,    89,    90,   955,
      48,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   964,    -1,
      -1,    -1,   968,    -1,   970,   971,    -1,    49,    50,    51,
      -1,    -1,    54,   115,    -1,   981,    -1,   119,    -1,    -1,
      -1,   712,    -1,    -1,   126,    -1,    -1,   129,    -1,    -1,
      -1,    89,    90,    -1,    76,    -1,   138,    -1,    80,    -1,
      -1,   732,   733,   734,    -1,   736,    88,    89,    90,    -1,
    1016,   742,    -1,    -1,  1020,    -1,    -1,   115,    -1,    -1,
      -1,   119,    -1,    -1,    -1,     1,    -1,     3,     4,   924,
      -1,    -1,     8,     9,    10,    -1,    -1,    -1,    -1,    -1,
     138,  1047,    -1,    -1,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,   889,    -1,    43,    44,   893,
     894,    47,    -1,    49,    50,    51,    52,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      -1,    77,    78,    79,    -1,    81,    82,    83,    84,    85,
      86,    87,    -1,    89,    90,    91,    -1,    -1,    -1,    95,
       6,    97,    98,    99,   100,   101,   102,    13,    14,    15,
      16,    17,    18,    19,    20,    -1,    -1,   113,    -1,   115,
      -1,    -1,   118,   119,   120,    -1,   970,   971,    -1,   880,
     881,    -1,    -1,    -1,    -1,     3,    -1,    -1,     6,     7,
      -1,     3,   138,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    -1,    -1,    -1,    -1,    -1,    40,    -1,
      48,    49,    50,    51,    52,    53,    48,    55,   107,   108,
     109,   110,    -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    -1,   125,   126,   127,   128,
     129,   130,   131,   132,   133,   134,   135,   136,    86,    -1,
      -1,    89,    90,     3,    -1,    -1,    -1,    89,    90,   980,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   999,    -1,
     118,   119,    -1,   115,    -1,    -1,    -1,   119,   126,    -1,
      40,   129,    -1,    -1,   132,   133,   134,    -1,    48,     3,
     138,   139,     6,     7,    -1,    41,   138,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    -1,    89,
      90,    -1,    -1,    -1,    -1,    49,    50,    51,    52,    53,
      -1,    55,    -1,    -1,    58,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,    70,    71,    72,   119,
      -1,   107,   108,   109,   110,    -1,   112,    -1,   107,   108,
     109,   110,    86,    -1,    -1,    89,    90,    -1,   138,   125,
     126,   127,   128,   129,   130,   131,   132,   133,   134,   135,
     136,   130,   131,   132,   133,   134,   135,   136,    -1,    -1,
      -1,   115,    -1,    -1,    -1,   119,    -1,    -1,    -1,    -1,
      -1,    -1,   126,    -1,    -1,   129,    -1,    -1,   132,   133,
     134,    -1,    -1,     3,   138,   139,     6,     7,    41,    -1,
      -1,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,
      50,    51,    52,    53,    -1,    55,    -1,    -1,    58,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    -1,   107,   108,   109,   110,    -1,   112,
      -1,    -1,   107,   108,   109,   110,    86,    -1,    -1,    89,
      90,    -1,   125,   126,   127,   128,   129,   130,   131,   132,
     133,   134,   135,   136,   129,   130,   131,   132,   133,   134,
     135,   136,    -1,    -1,    -1,   115,    -1,    -1,    -1,   119,
      -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,   129,
      -1,    -1,   132,   133,   134,    -1,    -1,     3,   138,   139,
       6,     7,    -1,    43,    -1,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    50,    51,    52,    53,    -1,    55,
      -1,    -1,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,   107,   108,   109,
     110,    -1,   112,    -1,    -1,    -1,   107,   108,   109,   110,
      86,    -1,    -1,    89,    90,   125,   126,   127,   128,   129,
     130,   131,   132,   133,   134,   135,   136,   128,   129,   130,
     131,   132,   133,   134,   135,   136,    -1,    -1,    -1,   115,
      -1,    -1,    -1,   119,    -1,    -1,    -1,    -1,    -1,    -1,
     126,    -1,    -1,   129,    -1,    -1,   132,   133,   134,    -1,
      -1,     3,   138,   139,     6,     7,    -1,    -1,    -1,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    -1,
      76,    -1,    -1,    -1,    -1,    -1,    -1,    49,    50,    51,
      52,    53,    -1,    55,    -1,    -1,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,   107,   108,   109,   110,    -1,   112,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    86,    -1,    -1,    89,    90,   125,
     126,   127,   128,   129,   130,   131,   132,   133,   134,   135,
     136,    -1,    -1,    -1,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,   115,    31,    -1,    33,   119,    -1,    36,
      37,    38,    39,    -1,   126,    -1,    -1,   129,    -1,    -1,
     132,   133,   134,    -1,    -1,     3,   138,   139,     6,     7,
      -1,    -1,    -1,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    49,    50,    51,    52,    53,    -1,    55,   107,   108,
     109,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    -1,   125,   126,   127,   128,
     129,   130,   131,   132,   133,   134,   135,   136,    86,    -1,
      -1,    89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,
      -1,   119,    -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,
      -1,   129,    -1,    -1,   132,   133,   134,    -1,    -1,     3,
     138,   139,     6,     7,    -1,    -1,    -1,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    49,    50,    51,    52,    53,
      -1,    55,   107,   108,   109,   110,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    -1,
      -1,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,    86,    -1,    -1,    89,    90,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,    -1,    -1,    -1,   119,    -1,    -1,    -1,    -1,
      -1,    -1,   126,    -1,    -1,   129,    -1,    -1,   132,   133,
     134,    -1,    -1,     3,   138,   139,     6,     7,    -1,    -1,
      -1,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,
      50,    51,    52,    53,    -1,    55,   107,   108,   109,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    -1,    -1,    -1,   127,   128,   129,   130,
     131,   132,   133,   134,   135,   136,    86,    -1,    -1,    89,
      90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,    -1,   119,
      -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,   129,
      -1,    -1,   132,   133,   134,    -1,    -1,     3,   138,   139,
       6,     7,    -1,    -1,    -1,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    50,    51,    52,    53,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    -1,    -1,     3,
      -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      86,    -1,    -1,    89,    90,    -1,    -1,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    -1,    40,   115,
      -1,    -1,    -1,   119,    48,    49,    50,    51,    52,    53,
      -1,    -1,    -1,    57,    -1,    57,   132,   133,   134,    -1,
      -1,    -1,   138,   139,    -1,    -1,    70,    71,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    86,    -1,    -1,    89,    90,    89,    90,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,     3,   115,   118,   119,    -1,   119,    -1,    -1,
      -1,    -1,   126,    -1,   126,   129,    -1,   129,    -1,    -1,
     134,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    -1,    -1,    -1,    57,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    -1,    -1,     3,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    86,    -1,    -1,    89,    90,
      -1,    -1,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,   115,    -1,    -1,   118,   119,    48,
      49,    50,    51,    52,    53,   126,    -1,    -1,   129,    -1,
      -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,     3,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    86,    -1,    -1,
      89,    90,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,   118,
     119,    -1,    -1,    52,    53,    -1,    -1,   126,    -1,    -1,
     129,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,     0,     1,    -1,     3,     4,    -1,
      -1,    -1,     8,     9,    10,    -1,    -1,    86,    -1,    -1,
      89,    90,    -1,    -1,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,    -1,   115,    43,    44,    -1,
     119,    -1,    -1,    49,    50,    51,    52,    53,    -1,    -1,
      56,    -1,    -1,    -1,    -1,   134,    62,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,    -1,    -1,    -1,
      -1,    77,    78,    79,    -1,    81,    82,    -1,    84,    85,
      86,    87,    -1,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,     1,    -1,     3,
       4,    -1,    -1,    -1,     8,     9,    10,   113,    -1,   115,
      -1,    -1,   118,   119,    -1,    -1,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    -1,    -1,    -1,    43,
      44,    -1,    -1,    47,    -1,    49,    50,    51,    52,    53,
      -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    -1,
      -1,    -1,    -1,    77,    78,    79,    -1,    81,    82,    -1,
      84,    85,    86,    87,    -1,    89,    90,    91,    92,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,     1,
      -1,     3,     4,    -1,    -1,    -1,     8,     9,    10,   113,
      -1,   115,    -1,    -1,   118,   119,    -1,    -1,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    -1,    -1,
      -1,    43,    44,    -1,    -1,    47,    -1,    49,    50,    51,
      52,    53,    -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    -1,    -1,    -1,    -1,    77,    78,    79,    -1,    81,
      82,    -1,    84,    85,    86,    87,    -1,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,     1,    -1,     3,     4,    -1,    -1,    -1,     8,     9,
      10,   113,    -1,   115,    -1,    -1,   118,   119,    -1,    -1,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      -1,    -1,    -1,    43,    44,    -1,    -1,    47,    -1,    49,
      50,    51,    52,    53,    -1,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    -1,    -1,    -1,    -1,    77,    78,    79,
      -1,    81,    82,    -1,    84,    85,    86,    87,    -1,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,     1,    -1,     3,     4,    -1,    -1,    -1,
       8,     9,    10,   113,    -1,   115,    -1,    -1,   118,   119,
      -1,    -1,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    -1,    -1,    -1,    43,    44,    -1,    -1,    -1,
      -1,    49,    50,    51,    52,    53,    -1,    -1,    56,    -1,
      -1,    -1,    60,    -1,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    -1,    -1,    -1,    -1,    77,
      78,    79,    -1,    81,    82,    -1,    84,    85,    86,    87,
      -1,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,     1,    -1,     3,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   113,    -1,   115,    -1,    -1,
     118,   119,    -1,    -1,    -1,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,     1,    -1,     3,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      86,    -1,    -1,    89,    90,    -1,    -1,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    -1,    -1,    -1,   115,
      -1,    -1,   118,   119,    48,    49,    50,    51,    52,    53,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,     1,
      -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    86,    -1,    -1,    89,    90,    -1,    -1,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    -1,    -1,
      -1,   115,    -1,    -1,   118,   119,    -1,    49,    50,    51,
      52,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    86,    -1,    -1,    89,    90,    -1,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,   107,
     108,   109,   110,   115,   112,    -1,    -1,   119,    49,    50,
      51,    52,    53,    -1,    -1,    -1,    -1,   125,   126,   127,
     128,   129,   130,   131,   132,   133,   134,   135,   136,    70,
      71,    72,    -1,    -1,     3,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    86,    87,    -1,    89,    90,
      -1,    -1,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,   115,    -1,    -1,    -1,   119,   120,
      49,    50,    51,    52,    53,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    -1,    -1,    -1,    -1,    -1,     3,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    86,    87,    13,
      89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,   115,    -1,    -1,    -1,
     119,   120,    -1,    -1,    48,    49,    50,    51,    52,    53,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    -1,
      -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    86,    -1,    -1,    89,    90,    -1,    -1,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    -1,    -1,
      -1,   115,    -1,    -1,   118,   119,    48,    49,    50,    51,
      52,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    86,    -1,    -1,    89,    90,    -1,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      -1,    -1,    -1,   115,    -1,    -1,   118,   119,    49,    50,
      51,    52,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    86,    -1,    -1,    89,    90,
      -1,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
     107,   108,   109,   110,   115,   112,    -1,    -1,   119,    49,
      50,    51,    52,    53,    -1,    -1,    -1,    -1,   125,   126,
     127,   128,   129,   130,   131,   132,   133,   134,   135,   136,
      70,    71,    72,    -1,     3,    -1,    -1,    -1,    58,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    86,    87,    -1,    89,
      90,    -1,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,   115,    -1,    -1,    -1,   119,
      49,    50,    51,    52,    53,    -1,    -1,   107,   108,   109,
     110,    -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,     3,   125,   126,   127,   128,   129,
     130,   131,   132,   133,   134,   135,   136,    86,    -1,    -1,
      89,    90,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,    -1,
     119,    -1,    -1,    52,    53,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    58,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,     3,    58,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    86,    -1,    -1,
      89,    90,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,   107,   108,   109,   110,   115,   112,    -1,    -1,
     119,    -1,    -1,    -1,   107,   108,   109,   110,    -1,   112,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   125,   126,   127,   128,   129,   130,   131,   132,
     133,   134,   135,   136,    -1,    -1,    -1,    86,    -1,    -1,
      89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,    -1,
     119
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   121,   122,   123,   142,   143,   307,     1,     3,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    48,    49,    50,
      51,    52,    53,    70,    71,    72,    86,    89,    90,   115,
     118,   119,   193,   235,   249,   250,   252,   253,   254,   255,
     256,   257,   282,   283,   293,   296,   298,     1,   235,     1,
      40,     0,     1,     4,     8,     9,    10,    21,    43,    44,
      56,    62,    63,    64,    65,    66,    67,    68,    69,    77,
      78,    79,    81,    82,    84,    85,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   113,   118,
     144,   145,   146,   148,   149,   150,   151,   152,   155,   156,
     158,   159,   160,   161,   162,   163,   164,   167,   168,   169,
     172,   174,   179,   180,   181,   182,   184,   188,   195,   196,
     197,   198,   199,   203,   204,   211,   212,   223,   231,   307,
     103,   292,   307,    48,    52,    71,    48,    48,    40,   140,
     103,   296,    43,   253,   249,    40,    48,    54,    57,    76,
     119,   126,   129,   134,   138,   240,   241,   243,   245,   246,
     247,   248,   296,   307,   249,   256,   296,   292,   117,   140,
     297,    43,    43,   232,   233,   235,   307,   120,    40,     6,
      85,   118,   301,    40,   304,   307,     1,   251,   252,   293,
      40,   304,    40,   166,   307,    40,    40,    84,    85,    40,
      84,    77,    82,    44,    77,    92,   296,    46,   293,   296,
      40,     4,    46,    40,    40,    43,    46,     4,   301,    40,
     178,   251,   176,   178,    40,    40,   301,    40,   103,   283,
     304,    40,   126,   129,   243,   248,   296,    87,   193,   251,
     283,     7,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    40,    55,   126,   129,   132,   133,   134,   138,
     139,   235,   236,   237,   239,   251,   252,   268,   269,   270,
     271,   301,   302,   307,    48,    48,    48,   296,   119,   298,
     283,   249,    40,   126,   129,   232,   246,   248,   296,    48,
      46,   105,   106,   258,   259,   260,   261,   262,    58,   268,
     269,   268,     3,    40,    48,   138,   244,   247,   296,    48,
     244,   247,   248,   249,   296,   240,    40,    57,   240,    40,
      57,    48,   126,   129,   244,   247,   296,   116,   298,   119,
     298,    41,    42,   234,   307,   260,   293,   294,   301,   283,
       6,    46,    85,   294,   305,   294,    43,    40,   243,    54,
      41,   294,   296,   293,   293,   294,    13,   173,   232,   232,
     296,    43,    54,   214,    54,    46,   293,   175,   305,   293,
     232,    46,   242,   243,   246,   307,    43,    42,   177,   307,
     294,   295,   307,   153,   154,   301,   232,   207,   208,   209,
     235,   282,   307,   296,   301,   126,   129,   248,   293,   296,
     305,    40,   294,   126,   129,   296,   116,   243,   296,   263,
     293,   307,    40,   243,    76,   274,   275,   296,   307,   268,
      40,    48,   268,   268,   268,   268,   268,   268,   268,   104,
      42,   238,   307,    40,   107,   108,   109,   110,   112,   125,
     126,   127,   128,   129,   130,   131,   132,   133,   134,   135,
     136,     7,    48,    48,    41,   297,   104,   126,   129,   248,
     296,   245,   296,   245,    41,    41,   126,   129,   245,   296,
     116,    48,    57,   268,    58,    40,   248,   296,    48,   296,
      40,    57,    48,   248,   232,    58,   268,   232,    58,   268,
      48,    48,   244,   247,    48,   244,   247,   116,    48,   126,
     129,   244,   297,    43,   235,    41,   296,   183,    42,    54,
      41,   240,   258,    41,    46,    41,    54,    41,    42,   171,
      42,    41,    41,    43,   143,   296,   213,    41,    41,    41,
      41,   176,   178,    41,    41,    42,    46,    41,   104,    42,
     210,   307,    59,   116,    41,   248,   296,    43,   116,   111,
      54,    76,   194,   307,   232,   296,    80,    88,    89,    90,
     186,   240,   249,   285,   286,   276,    46,    43,   274,    41,
     126,   129,   134,   248,   251,    48,   239,   268,   268,   268,
     268,   268,   268,   268,   268,   268,   268,   268,   268,   268,
     268,   268,   268,   268,   283,   296,   116,    41,    41,    41,
     116,   245,   245,   268,   232,   244,   296,    41,   116,    48,
     232,    58,   268,    48,    41,    58,    41,    58,    48,    48,
      48,    48,   126,   129,   244,   247,    48,    48,    48,   244,
     234,     4,    46,   301,   143,   305,   153,   270,   301,   306,
      43,    43,   147,     4,   165,   301,     4,    43,    46,   114,
     170,   243,   301,   303,   294,   301,   306,    41,   235,   243,
      46,    47,    43,   143,    44,   231,   176,    43,    46,    40,
      47,   177,   115,   119,   293,   299,    43,   305,   235,   170,
      91,   205,   209,   157,   243,   301,   116,    30,    32,    34,
      35,   187,   254,   255,   296,    57,   189,   253,    43,    46,
      41,    40,    40,   285,    90,    89,     1,    42,    43,    46,
     185,   240,   286,   240,    78,   277,   278,   284,   307,   201,
      46,   268,    41,    41,   134,   249,    41,   126,   129,   241,
      48,   238,    76,   296,    41,    58,    41,    41,   244,   244,
      41,    58,   244,   244,    48,    48,    48,    48,    48,   244,
      48,    48,    48,    47,    42,    42,     1,    43,    66,    73,
      74,    75,    78,    83,   138,   148,   149,   150,   151,   155,
     156,   160,   162,   164,   167,   169,   172,   174,   179,   180,
     181,   182,   199,   203,   204,   211,   215,   219,   220,   221,
     222,   223,   224,   225,   226,   229,   231,   307,    40,   249,
     286,   287,   307,    54,    41,    42,   171,   170,   243,   287,
      47,   301,   251,   293,    43,    54,   303,   232,   140,   117,
     140,   300,   103,    41,    47,   296,    44,   118,   184,   199,
     203,   204,   206,   220,   222,   224,   231,   210,   143,   287,
      43,   292,   186,    40,    46,   190,   150,   264,   265,   307,
      40,    54,   286,   287,   288,   232,   268,   243,   240,    42,
      73,    74,    75,   279,   281,   215,   200,   268,   268,   268,
      41,    41,    41,    40,   268,   240,    41,   244,   244,    48,
      48,    48,   244,    48,    48,   305,   305,   218,    46,    76,
      76,    76,   138,    40,   298,    47,   215,    30,    32,    34,
      35,   120,   230,   251,   255,   296,   232,   286,   170,   301,
     306,    43,   243,    41,   287,    43,   243,    43,   178,    41,
     119,   293,   293,   119,   293,   236,     4,    46,    54,   103,
      87,    60,    43,   185,   232,    40,    43,   191,   266,   293,
      42,    47,   232,   258,    54,    76,   289,   307,    41,    41,
     186,   278,   296,   280,    47,   215,   268,   268,   251,   244,
      48,    48,   244,   244,   215,   216,   298,    40,   292,   251,
      76,    40,    43,    41,   171,   287,    43,   243,   170,    43,
      43,   300,   300,   104,   293,   207,    41,   192,   264,    54,
     264,    43,   243,    41,    43,   260,   290,   291,   296,    43,
      46,   185,    48,   272,   273,   307,   284,    43,   202,   243,
      47,   241,   244,   244,   215,    40,   232,    40,   126,   129,
     248,   268,   232,    43,    43,   287,    43,   104,   287,    43,
     267,   268,   266,   186,    43,    46,    43,    42,    48,    40,
      46,    48,   296,   186,   202,    41,    47,   232,    41,   232,
      40,    40,    40,   129,    43,    41,    43,   111,   190,   264,
     185,   291,    48,    48,   273,   185,   217,    41,   227,   287,
      41,   232,   232,   232,    40,   288,   251,   191,    48,    48,
     215,   228,   287,    43,    46,    54,   228,    41,    41,    41,
     232,   190,    48,    43,    46,    54,   260,   228,   228,   228,
      41,   191,    48,   258,    43,   228,    43
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   141,   142,   142,   142,   142,   142,   142,   142,   143,
     143,   144,   144,   144,   144,   144,   144,   144,   145,   145,
     145,   145,   145,   145,   145,   145,   145,   145,   145,   145,
     145,   145,   145,   145,   145,   145,   145,   145,   145,   147,
     146,   148,   149,   150,   150,   150,   151,   151,   152,   152,
     152,   152,   153,   154,   154,   155,   155,   155,   157,   156,
     158,   158,   159,   159,   160,   160,   160,   160,   161,   162,
     162,   163,   163,   164,   164,   165,   165,   166,   166,   167,
     167,   167,   168,   168,   169,   169,   169,   169,   169,   169,
     169,   169,   170,   170,   170,   171,   171,   172,   173,   173,
     174,   174,   174,   175,   176,   177,   177,   178,   178,   178,
     179,   180,   181,   182,   182,   182,   183,   182,   182,   182,
     182,   184,   184,   185,   185,   185,   185,   186,   186,   186,
     186,   187,   187,   187,   187,   187,   187,   187,   188,   188,
     188,   189,   190,   191,   192,   191,   193,   193,   193,   194,
     194,   195,   196,   196,   197,   198,   198,   198,   198,   198,
     198,   200,   199,   201,   199,   202,   202,   203,   205,   204,
     204,   204,   206,   206,   206,   206,   206,   206,   206,   207,
     208,   208,   209,   209,   210,   210,   211,   211,   213,   212,
     214,   212,   212,   215,   216,   217,   215,   215,   215,   218,
     215,   219,   219,   219,   219,   219,   219,   219,   219,   219,
     219,   219,   219,   219,   219,   219,   219,   219,   219,   219,
     220,   221,   221,   222,   222,   222,   222,   222,   223,   224,
     225,   225,   225,   226,   226,   226,   226,   226,   226,   226,
     226,   226,   226,   226,   227,   227,   227,   228,   228,   228,
     229,   230,   230,   230,   230,   230,   230,   231,   231,   231,
     231,   231,   231,   231,   231,   231,   231,   231,   231,   231,
     231,   231,   231,   231,   231,   231,   232,   233,   233,   234,
     234,   235,   235,   235,   236,   237,   237,   238,   238,   239,
     239,   240,   240,   240,   240,   240,   241,   241,   241,   242,
     242,   242,   243,   243,   243,   243,   243,   243,   243,   243,
     243,   243,   243,   243,   243,   243,   243,   243,   243,   243,
     243,   243,   243,   243,   244,   244,   244,   244,   244,   244,
     244,   244,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   246,   246,   246,   246,   246,   246,   246,   246,
     246,   246,   246,   246,   246,   246,   247,   247,   247,   247,
     247,   247,   247,   248,   248,   248,   248,   249,   249,   250,
     250,   250,   251,   252,   252,   252,   252,   253,   253,   253,
     253,   253,   253,   253,   253,   253,   254,   255,   256,   256,
     257,   257,   257,   257,   257,   257,   257,   257,   257,   257,
     257,   257,   257,   257,   259,   258,   258,   260,   260,   261,
     262,   263,   263,   264,   264,   265,   265,   265,   265,   266,
     266,   267,   268,   268,   269,   269,   269,   269,   269,   269,
     269,   269,   269,   269,   269,   269,   269,   269,   269,   269,
     269,   269,   270,   270,   270,   270,   270,   270,   270,   270,
     271,   271,   271,   271,   271,   271,   271,   271,   271,   271,
     271,   271,   271,   271,   271,   271,   271,   271,   271,   271,
     271,   271,   272,   273,   273,   274,   276,   275,   275,   277,
     277,   279,   278,   280,   278,   281,   281,   281,   282,   282,
     282,   282,   283,   283,   283,   284,   284,   285,   285,   285,
     285,   286,   286,   286,   286,   286,   287,   287,   287,   287,
     288,   288,   288,   288,   288,   288,   289,   289,   290,   290,
     290,   290,   291,   291,   292,   292,   293,   293,   293,   294,
     294,   294,   295,   295,   296,   296,   296,   296,   296,   296,
     297,   297,   297,   297,   298,   299,   299,   299,   299,   299,
     299,   300,   300,   300,   300,   301,   301,   302,   302,   303,
     303,   303,   304,   304,   305,   305,   305,   305,   305,   305,
     306,   306,   307
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     3,     2,     3,     2,     5,     3,     2,
       1,     1,     1,     1,     1,     1,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       7,     5,     3,     5,     5,     3,     2,     2,     5,     2,
       5,     2,     4,     1,     1,     7,     7,     5,     0,     7,
       1,     1,     2,     2,     1,     5,     5,     5,     3,     4,
       3,     7,     8,     5,     3,     1,     1,     3,     1,     4,
       7,     6,     1,     1,     7,     9,     8,    10,     5,     7,
       6,     8,     1,     1,     5,     4,     5,     7,     1,     3,
       6,     6,     8,     1,     2,     3,     1,     2,     3,     6,
       5,     9,     2,     1,     1,     1,     0,     6,     1,     3,
       8,     5,     7,     1,     4,     1,     1,     1,     2,     2,
       3,     1,     1,     1,     2,     1,     1,     1,    11,    13,
       7,     1,     1,     1,     0,     3,     1,     2,     2,     2,
       1,     5,     8,    10,     6,     1,     1,     1,     1,     1,
       1,     0,     9,     0,     8,     1,     3,     4,     0,     6,
       3,     4,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     1,     1,     1,     3,     1,     3,     4,     0,     6,
       0,     5,     5,     2,     0,     0,     7,     1,     1,     0,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       6,     6,     7,     8,     8,     8,     9,     7,     5,     2,
       2,     2,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     2,     4,     2,     2,     4,     2,
       5,     1,     1,     1,     2,     1,     1,     1,     2,     3,
       1,     1,     1,     1,     1,     1,     2,     2,     2,     2,
       1,     2,     2,     2,     2,     1,     1,     2,     1,     3,
       1,     2,     7,     3,     1,     2,     1,     3,     1,     1,
       1,     2,     5,     2,     2,     1,     2,     2,     1,     1,
       1,     1,     2,     3,     3,     1,     2,     2,     3,     4,
       5,     4,     5,     6,     6,     4,     5,     5,     6,     7,
       8,     8,     7,     7,     1,     2,     3,     4,     5,     3,
       4,     4,     1,     2,     4,     4,     4,     5,     3,     4,
       4,     5,     1,     2,     2,     2,     3,     3,     1,     2,
       2,     1,     1,     2,     3,     4,     3,     4,     2,     3,
       3,     4,     3,     3,     2,     2,     1,     1,     2,     1,
       1,     1,     1,     2,     1,     2,     3,     1,     1,     1,
       2,     2,     1,     1,     2,     1,     4,     1,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     5,     3,     3,     1,     1,
       3,     1,     1,     1,     1,     1,     5,     8,     1,     1,
       1,     1,     3,     4,     5,     5,     5,     6,     6,     2,
       2,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     5,     2,     2,     2,
       2,     2,     3,     1,     1,     1,     0,     3,     1,     1,
       3,     0,     4,     0,     6,     1,     1,     1,     1,     1,
       4,     4,     1,     1,     1,     1,     1,     1,     1,     2,
       2,     4,     1,     1,     2,     4,     1,     1,     2,     1,
       3,     3,     4,     4,     3,     4,     2,     1,     1,     3,
       4,     6,     2,     2,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     2,     4,     1,     3,     1,     3,
       3,     2,     2,     2,     2,     2,     4,     1,     3,     1,
       3,     3,     2,     2,     2,     2,     1,     2,     1,     1,
       1,     1,     3,     1,     3,     5,     1,     3,     3,     5,
       1,     1,     0
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 1464 "parser.y" /* yacc.c:1646  */
    {
                   if (!classes) classes = NewHash();
		   Setattr((yyvsp[0].node),"classes",classes); 
		   Setattr((yyvsp[0].node),"name",ModuleName);
		   
		   if ((!module_node) && ModuleName) {
		     module_node = new_node("module");
		     Setattr(module_node,"name",ModuleName);
		   }
		   Setattr((yyvsp[0].node),"module",module_node);
	           top = (yyvsp[0].node);
               }
#line 4427 "y.tab.c" /* yacc.c:1646  */
    break;

  case 3:
#line 1476 "parser.y" /* yacc.c:1646  */
    {
                 top = Copy(Getattr((yyvsp[-1].p),"type"));
		 Delete((yyvsp[-1].p));
               }
#line 4436 "y.tab.c" /* yacc.c:1646  */
    break;

  case 4:
#line 1480 "parser.y" /* yacc.c:1646  */
    {
                 top = 0;
               }
#line 4444 "y.tab.c" /* yacc.c:1646  */
    break;

  case 5:
#line 1483 "parser.y" /* yacc.c:1646  */
    {
                 top = (yyvsp[-1].p);
               }
#line 4452 "y.tab.c" /* yacc.c:1646  */
    break;

  case 6:
#line 1486 "parser.y" /* yacc.c:1646  */
    {
                 top = 0;
               }
#line 4460 "y.tab.c" /* yacc.c:1646  */
    break;

  case 7:
#line 1489 "parser.y" /* yacc.c:1646  */
    {
                 top = (yyvsp[-2].pl);
               }
#line 4468 "y.tab.c" /* yacc.c:1646  */
    break;

  case 8:
#line 1492 "parser.y" /* yacc.c:1646  */
    {
                 top = 0;
               }
#line 4476 "y.tab.c" /* yacc.c:1646  */
    break;

  case 9:
#line 1497 "parser.y" /* yacc.c:1646  */
    {  
                   /* add declaration to end of linked list (the declaration isn't always a single declaration, sometimes it is a linked list itself) */
                   appendChild((yyvsp[-1].node),(yyvsp[0].node));
                   (yyval.node) = (yyvsp[-1].node);
               }
#line 4486 "y.tab.c" /* yacc.c:1646  */
    break;

  case 10:
#line 1502 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.node) = new_node("top");
               }
#line 4494 "y.tab.c" /* yacc.c:1646  */
    break;

  case 11:
#line 1507 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4500 "y.tab.c" /* yacc.c:1646  */
    break;

  case 12:
#line 1508 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4506 "y.tab.c" /* yacc.c:1646  */
    break;

  case 13:
#line 1509 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4512 "y.tab.c" /* yacc.c:1646  */
    break;

  case 14:
#line 1510 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = 0; }
#line 4518 "y.tab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 1511 "parser.y" /* yacc.c:1646  */
    {
                  (yyval.node) = 0;
		  if (cparse_unknown_directive) {
		      Swig_error(cparse_file, cparse_line, "Unknown directive '%s'.\n", cparse_unknown_directive);
		  } else {
		      Swig_error(cparse_file, cparse_line, "Syntax error in input(1).\n");
		  }
		  exit(1);
               }
#line 4532 "y.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 1521 "parser.y" /* yacc.c:1646  */
    { 
                  if ((yyval.node)) {
   		      add_symbols((yyval.node));
                  }
                  (yyval.node) = (yyvsp[0].node); 
	       }
#line 4543 "y.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 1537 "parser.y" /* yacc.c:1646  */
    {
                  (yyval.node) = 0;
                  skip_decl();
               }
#line 4552 "y.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 1547 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4558 "y.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 1548 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4564 "y.tab.c" /* yacc.c:1646  */
    break;

  case 20:
#line 1549 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4570 "y.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 1550 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4576 "y.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 1551 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4582 "y.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 1552 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4588 "y.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 1553 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4594 "y.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 1554 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4600 "y.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 1555 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4606 "y.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 1556 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4612 "y.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 1557 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4618 "y.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 1558 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4624 "y.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 1559 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4630 "y.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 1560 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4636 "y.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 1561 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4642 "y.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 1562 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4648 "y.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 1563 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4654 "y.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 1564 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4660 "y.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 1565 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4666 "y.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 1566 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4672 "y.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 1567 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 4678 "y.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 1574 "parser.y" /* yacc.c:1646  */
    {
               Node *cls;
	       String *clsname;
	       extendmode = 1;
	       cplus_mode = CPLUS_PUBLIC;
	       if (!classes) classes = NewHash();
	       if (!classes_typedefs) classes_typedefs = NewHash();
	       clsname = make_class_name((yyvsp[-1].str));
	       cls = Getattr(classes,clsname);
	       if (!cls) {
	         cls = Getattr(classes_typedefs, clsname);
		 if (!cls) {
		   /* No previous definition. Create a new scope */
		   Node *am = Getattr(Swig_extend_hash(),clsname);
		   if (!am) {
		     Swig_symbol_newscope();
		     Swig_symbol_setscopename((yyvsp[-1].str));
		     prev_symtab = 0;
		   } else {
		     prev_symtab = Swig_symbol_setscope(Getattr(am,"symtab"));
		   }
		   current_class = 0;
		 } else {
		   /* Previous typedef class definition.  Use its symbol table.
		      Deprecated, just the real name should be used. 
		      Note that %extend before the class typedef never worked, only %extend after the class typdef. */
		   prev_symtab = Swig_symbol_setscope(Getattr(cls, "symtab"));
		   current_class = cls;
		   SWIG_WARN_NODE_BEGIN(cls);
		   Swig_warning(WARN_PARSE_EXTEND_NAME, cparse_file, cparse_line, "Deprecated %%extend name used - the %s name '%s' should be used instead of the typedef name '%s'.\n", Getattr(cls, "kind"), SwigType_namestr(Getattr(cls, "name")), (yyvsp[-1].str));
		   SWIG_WARN_NODE_END(cls);
		 }
	       } else {
		 /* Previous class definition.  Use its symbol table */
		 prev_symtab = Swig_symbol_setscope(Getattr(cls,"symtab"));
		 current_class = cls;
	       }
	       Classprefix = NewString((yyvsp[-1].str));
	       Namespaceprefix= Swig_symbol_qualifiedscopename(0);
	       Delete(clsname);
	     }
#line 4724 "y.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 1614 "parser.y" /* yacc.c:1646  */
    {
               String *clsname;
	       extendmode = 0;
               (yyval.node) = new_node("extend");
	       Setattr((yyval.node),"symtab",Swig_symbol_popscope());
	       if (prev_symtab) {
		 Swig_symbol_setscope(prev_symtab);
	       }
	       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
               clsname = make_class_name((yyvsp[-4].str));
	       Setattr((yyval.node),"name",clsname);

	       mark_nodes_as_extend((yyvsp[-1].node));
	       if (current_class) {
		 /* We add the extension to the previously defined class */
		 appendChild((yyval.node),(yyvsp[-1].node));
		 appendChild(current_class,(yyval.node));
	       } else {
		 /* We store the extensions in the extensions hash */
		 Node *am = Getattr(Swig_extend_hash(),clsname);
		 if (am) {
		   /* Append the members to the previous extend methods */
		   appendChild(am,(yyvsp[-1].node));
		 } else {
		   appendChild((yyval.node),(yyvsp[-1].node));
		   Setattr(Swig_extend_hash(),clsname,(yyval.node));
		 }
	       }
	       current_class = 0;
	       Delete(Classprefix);
	       Delete(clsname);
	       Classprefix = 0;
	       prev_symtab = 0;
	       (yyval.node) = 0;

	     }
#line 4765 "y.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 1656 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.node) = new_node("apply");
                    Setattr((yyval.node),"pattern",Getattr((yyvsp[-3].p),"pattern"));
		    appendChild((yyval.node),(yyvsp[-1].p));
               }
#line 4775 "y.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 1666 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.node) = new_node("clear");
		 appendChild((yyval.node),(yyvsp[-1].p));
               }
#line 4784 "y.tab.c" /* yacc.c:1646  */
    break;

  case 43:
#line 1677 "parser.y" /* yacc.c:1646  */
    {
		   if (((yyvsp[-1].dtype).type != T_ERROR) && ((yyvsp[-1].dtype).type != T_SYMBOL)) {
		     SwigType *type = NewSwigType((yyvsp[-1].dtype).type);
		     (yyval.node) = new_node("constant");
		     Setattr((yyval.node),"name",(yyvsp[-3].id));
		     Setattr((yyval.node),"type",type);
		     Setattr((yyval.node),"value",(yyvsp[-1].dtype).val);
		     if ((yyvsp[-1].dtype).rawval) Setattr((yyval.node),"rawval", (yyvsp[-1].dtype).rawval);
		     Setattr((yyval.node),"storage","%constant");
		     SetFlag((yyval.node),"feature:immutable");
		     add_symbols((yyval.node));
		     Delete(type);
		   } else {
		     if ((yyvsp[-1].dtype).type == T_ERROR) {
		       Swig_warning(WARN_PARSE_UNSUPPORTED_VALUE,cparse_file,cparse_line,"Unsupported constant value (ignored)\n");
		     }
		     (yyval.node) = 0;
		   }

	       }
#line 4809 "y.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 1698 "parser.y" /* yacc.c:1646  */
    {
		 if (((yyvsp[-1].dtype).type != T_ERROR) && ((yyvsp[-1].dtype).type != T_SYMBOL)) {
		   SwigType_push((yyvsp[-3].type),(yyvsp[-2].decl).type);
		   /* Sneaky callback function trick */
		   if (SwigType_isfunction((yyvsp[-3].type))) {
		     SwigType_add_pointer((yyvsp[-3].type));
		   }
		   (yyval.node) = new_node("constant");
		   Setattr((yyval.node),"name",(yyvsp[-2].decl).id);
		   Setattr((yyval.node),"type",(yyvsp[-3].type));
		   Setattr((yyval.node),"value",(yyvsp[-1].dtype).val);
		   if ((yyvsp[-1].dtype).rawval) Setattr((yyval.node),"rawval", (yyvsp[-1].dtype).rawval);
		   Setattr((yyval.node),"storage","%constant");
		   SetFlag((yyval.node),"feature:immutable");
		   add_symbols((yyval.node));
		 } else {
		     if ((yyvsp[-1].dtype).type == T_ERROR) {
		       Swig_warning(WARN_PARSE_UNSUPPORTED_VALUE,cparse_file,cparse_line,"Unsupported constant value\n");
		     }
		   (yyval.node) = 0;
		 }
               }
#line 4836 "y.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 1720 "parser.y" /* yacc.c:1646  */
    {
		 Swig_warning(WARN_PARSE_BAD_VALUE,cparse_file,cparse_line,"Bad constant value (ignored).\n");
		 (yyval.node) = 0;
	       }
#line 4845 "y.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 1731 "parser.y" /* yacc.c:1646  */
    {
		 char temp[64];
		 Replace((yyvsp[0].str),"$file",cparse_file, DOH_REPLACE_ANY);
		 sprintf(temp,"%d", cparse_line);
		 Replace((yyvsp[0].str),"$line",temp,DOH_REPLACE_ANY);
		 Printf(stderr,"%s\n", (yyvsp[0].str));
		 Delete((yyvsp[0].str));
                 (yyval.node) = 0;
	       }
#line 4859 "y.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 1740 "parser.y" /* yacc.c:1646  */
    {
		 char temp[64];
		 String *s = NewString((yyvsp[0].id));
		 Replace(s,"$file",cparse_file, DOH_REPLACE_ANY);
		 sprintf(temp,"%d", cparse_line);
		 Replace(s,"$line",temp,DOH_REPLACE_ANY);
		 Printf(stderr,"%s\n", s);
		 Delete(s);
                 (yyval.node) = 0;
               }
#line 4874 "y.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 1759 "parser.y" /* yacc.c:1646  */
    {
                    skip_balanced('{','}');
		    (yyval.node) = 0;
		    Swig_warning(WARN_DEPRECATED_EXCEPT,cparse_file, cparse_line, "%%except is deprecated.  Use %%exception instead.\n");
	       }
#line 4884 "y.tab.c" /* yacc.c:1646  */
    break;

  case 49:
#line 1765 "parser.y" /* yacc.c:1646  */
    {
                    skip_balanced('{','}');
		    (yyval.node) = 0;
		    Swig_warning(WARN_DEPRECATED_EXCEPT,cparse_file, cparse_line, "%%except is deprecated.  Use %%exception instead.\n");
               }
#line 4894 "y.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 1771 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.node) = 0;
		 Swig_warning(WARN_DEPRECATED_EXCEPT,cparse_file, cparse_line, "%%except is deprecated.  Use %%exception instead.\n");
               }
#line 4903 "y.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 1776 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.node) = 0;
		 Swig_warning(WARN_DEPRECATED_EXCEPT,cparse_file, cparse_line, "%%except is deprecated.  Use %%exception instead.\n");
	       }
#line 4912 "y.tab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 1783 "parser.y" /* yacc.c:1646  */
    {		 
                 (yyval.node) = NewHash();
                 Setattr((yyval.node),"value",(yyvsp[-3].id));
		 Setattr((yyval.node),"type",Getattr((yyvsp[-1].p),"type"));
               }
#line 4922 "y.tab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 1790 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.node) = NewHash();
                 Setattr((yyval.node),"value",(yyvsp[0].id));
              }
#line 4931 "y.tab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 1794 "parser.y" /* yacc.c:1646  */
    {
                (yyval.node) = (yyvsp[0].node);
              }
#line 4939 "y.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 1807 "parser.y" /* yacc.c:1646  */
    {
                   Hash *p = (yyvsp[-2].node);
		   (yyval.node) = new_node("fragment");
		   Setattr((yyval.node),"value",Getattr((yyvsp[-4].node),"value"));
		   Setattr((yyval.node),"type",Getattr((yyvsp[-4].node),"type"));
		   Setattr((yyval.node),"section",Getattr(p,"name"));
		   Setattr((yyval.node),"kwargs",nextSibling(p));
		   Setattr((yyval.node),"code",(yyvsp[0].str));
                 }
#line 4953 "y.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 1816 "parser.y" /* yacc.c:1646  */
    {
		   Hash *p = (yyvsp[-2].node);
		   String *code;
                   skip_balanced('{','}');
		   (yyval.node) = new_node("fragment");
		   Setattr((yyval.node),"value",Getattr((yyvsp[-4].node),"value"));
		   Setattr((yyval.node),"type",Getattr((yyvsp[-4].node),"type"));
		   Setattr((yyval.node),"section",Getattr(p,"name"));
		   Setattr((yyval.node),"kwargs",nextSibling(p));
		   Delitem(scanner_ccode,0);
		   Delitem(scanner_ccode,DOH_END);
		   code = Copy(scanner_ccode);
		   Setattr((yyval.node),"code",code);
		   Delete(code);
                 }
#line 4973 "y.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 1831 "parser.y" /* yacc.c:1646  */
    {
		   (yyval.node) = new_node("fragment");
		   Setattr((yyval.node),"value",Getattr((yyvsp[-2].node),"value"));
		   Setattr((yyval.node),"type",Getattr((yyvsp[-2].node),"type"));
		   Setattr((yyval.node),"emitonly","1");
		 }
#line 4984 "y.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 1844 "parser.y" /* yacc.c:1646  */
    {
                     (yyvsp[-3].loc).filename = Copy(cparse_file);
		     (yyvsp[-3].loc).line = cparse_line;
		     scanner_set_location(NewString((yyvsp[-1].id)),1);
                     if ((yyvsp[-2].node)) { 
		       String *maininput = Getattr((yyvsp[-2].node), "maininput");
		       if (maininput)
		         scanner_set_main_input_file(NewString(maininput));
		     }
               }
#line 4999 "y.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 1853 "parser.y" /* yacc.c:1646  */
    {
                     String *mname = 0;
                     (yyval.node) = (yyvsp[-1].node);
		     scanner_set_location((yyvsp[-6].loc).filename,(yyvsp[-6].loc).line+1);
		     if (strcmp((yyvsp[-6].loc).type,"include") == 0) set_nodeType((yyval.node),"include");
		     if (strcmp((yyvsp[-6].loc).type,"import") == 0) {
		       mname = (yyvsp[-5].node) ? Getattr((yyvsp[-5].node),"module") : 0;
		       set_nodeType((yyval.node),"import");
		       if (import_mode) --import_mode;
		     }
		     
		     Setattr((yyval.node),"name",(yyvsp[-4].id));
		     /* Search for the module (if any) */
		     {
			 Node *n = firstChild((yyval.node));
			 while (n) {
			     if (Strcmp(nodeType(n),"module") == 0) {
			         if (mname) {
				   Setattr(n,"name", mname);
				   mname = 0;
				 }
				 Setattr((yyval.node),"module",Getattr(n,"name"));
				 break;
			     }
			     n = nextSibling(n);
			 }
			 if (mname) {
			   /* There is no module node in the import
			      node, ie, you imported a .h file
			      directly.  We are forced then to create
			      a new import node with a module node.
			   */			      
			   Node *nint = new_node("import");
			   Node *mnode = new_node("module");
			   Setattr(mnode,"name", mname);
                           Setattr(mnode,"options",(yyvsp[-5].node));
			   appendChild(nint,mnode);
			   Delete(mnode);
			   appendChild(nint,firstChild((yyval.node)));
			   (yyval.node) = nint;
			   Setattr((yyval.node),"module",mname);
			 }
		     }
		     Setattr((yyval.node),"options",(yyvsp[-5].node));
               }
#line 5049 "y.tab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 1900 "parser.y" /* yacc.c:1646  */
    { (yyval.loc).type = "include"; }
#line 5055 "y.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 1901 "parser.y" /* yacc.c:1646  */
    { (yyval.loc).type = "import"; ++import_mode;}
#line 5061 "y.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 1908 "parser.y" /* yacc.c:1646  */
    {
                 String *cpps;
		 if (Namespaceprefix) {
		   Swig_error(cparse_file, cparse_start_line, "%%inline directive inside a namespace is disallowed.\n");
		   (yyval.node) = 0;
		 } else {
		   (yyval.node) = new_node("insert");
		   Setattr((yyval.node),"code",(yyvsp[0].str));
		   /* Need to run through the preprocessor */
		   Seek((yyvsp[0].str),0,SEEK_SET);
		   Setline((yyvsp[0].str),cparse_start_line);
		   Setfile((yyvsp[0].str),cparse_file);
		   cpps = Preprocessor_parse((yyvsp[0].str));
		   start_inline(Char(cpps), cparse_start_line);
		   Delete((yyvsp[0].str));
		   Delete(cpps);
		 }
		 
	       }
#line 5085 "y.tab.c" /* yacc.c:1646  */
    break;

  case 63:
#line 1927 "parser.y" /* yacc.c:1646  */
    {
                 String *cpps;
		 int start_line = cparse_line;
		 skip_balanced('{','}');
		 if (Namespaceprefix) {
		   Swig_error(cparse_file, cparse_start_line, "%%inline directive inside a namespace is disallowed.\n");
		   
		   (yyval.node) = 0;
		 } else {
		   String *code;
                   (yyval.node) = new_node("insert");
		   Delitem(scanner_ccode,0);
		   Delitem(scanner_ccode,DOH_END);
		   code = Copy(scanner_ccode);
		   Setattr((yyval.node),"code", code);
		   Delete(code);		   
		   cpps=Copy(scanner_ccode);
		   start_inline(Char(cpps), start_line);
		   Delete(cpps);
		 }
               }
#line 5111 "y.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 1958 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.node) = new_node("insert");
		 Setattr((yyval.node),"code",(yyvsp[0].str));
	       }
#line 5120 "y.tab.c" /* yacc.c:1646  */
    break;

  case 65:
#line 1962 "parser.y" /* yacc.c:1646  */
    {
		 String *code = NewStringEmpty();
		 (yyval.node) = new_node("insert");
		 Setattr((yyval.node),"section",(yyvsp[-2].id));
		 Setattr((yyval.node),"code",code);
		 if (Swig_insert_file((yyvsp[0].id),code) < 0) {
		   Swig_error(cparse_file, cparse_line, "Couldn't find '%s'.\n", (yyvsp[0].id));
		   (yyval.node) = 0;
		 } 
               }
#line 5135 "y.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 1972 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.node) = new_node("insert");
		 Setattr((yyval.node),"section",(yyvsp[-2].id));
		 Setattr((yyval.node),"code",(yyvsp[0].str));
               }
#line 5145 "y.tab.c" /* yacc.c:1646  */
    break;

  case 67:
#line 1977 "parser.y" /* yacc.c:1646  */
    {
		 String *code;
                 skip_balanced('{','}');
		 (yyval.node) = new_node("insert");
		 Setattr((yyval.node),"section",(yyvsp[-2].id));
		 Delitem(scanner_ccode,0);
		 Delitem(scanner_ccode,DOH_END);
		 code = Copy(scanner_ccode);
		 Setattr((yyval.node),"code", code);
		 Delete(code);
	       }
#line 5161 "y.tab.c" /* yacc.c:1646  */
    break;

  case 68:
#line 1995 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.node) = new_node("module");
		 if ((yyvsp[-1].node)) {
		   Setattr((yyval.node),"options",(yyvsp[-1].node));
		   if (Getattr((yyvsp[-1].node),"directors")) {
		     Wrapper_director_mode_set(1);
		     if (!cparse_cplusplus) {
		       Swig_error(cparse_file, cparse_line, "Directors are not supported for C code and require the -c++ option\n");
		     }
		   } 
		   if (Getattr((yyvsp[-1].node),"dirprot")) {
		     Wrapper_director_protected_mode_set(1);
		   } 
		   if (Getattr((yyvsp[-1].node),"allprotected")) {
		     Wrapper_all_protected_mode_set(1);
		   } 
		   if (Getattr((yyvsp[-1].node),"templatereduce")) {
		     template_reduce = 1;
		   }
		   if (Getattr((yyvsp[-1].node),"notemplatereduce")) {
		     template_reduce = 0;
		   }
		 }
		 if (!ModuleName) ModuleName = NewString((yyvsp[0].id));
		 if (!import_mode) {
		   /* first module included, we apply global
		      ModuleName, which can be modify by -module */
		   String *mname = Copy(ModuleName);
		   Setattr((yyval.node),"name",mname);
		   Delete(mname);
		 } else { 
		   /* import mode, we just pass the idstring */
		   Setattr((yyval.node),"name",(yyvsp[0].id));   
		 }		 
		 if (!module_node) module_node = (yyval.node);
	       }
#line 5202 "y.tab.c" /* yacc.c:1646  */
    break;

  case 69:
#line 2038 "parser.y" /* yacc.c:1646  */
    {
                 Swig_warning(WARN_DEPRECATED_NAME,cparse_file,cparse_line, "%%name is deprecated.  Use %%rename instead.\n");
		 Delete(yyrename);
                 yyrename = NewString((yyvsp[-1].id));
		 (yyval.node) = 0;
               }
#line 5213 "y.tab.c" /* yacc.c:1646  */
    break;

  case 70:
#line 2044 "parser.y" /* yacc.c:1646  */
    {
		 Swig_warning(WARN_DEPRECATED_NAME,cparse_file,cparse_line, "%%name is deprecated.  Use %%rename instead.\n");
		 (yyval.node) = 0;
		 Swig_error(cparse_file,cparse_line,"Missing argument to %%name directive.\n");
	       }
#line 5223 "y.tab.c" /* yacc.c:1646  */
    break;

  case 71:
#line 2057 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.node) = new_node("native");
		 Setattr((yyval.node),"name",(yyvsp[-4].id));
		 Setattr((yyval.node),"wrap:name",(yyvsp[-1].id));
	         add_symbols((yyval.node));
	       }
#line 5234 "y.tab.c" /* yacc.c:1646  */
    break;

  case 72:
#line 2063 "parser.y" /* yacc.c:1646  */
    {
		 if (!SwigType_isfunction((yyvsp[-1].decl).type)) {
		   Swig_error(cparse_file,cparse_line,"%%native declaration '%s' is not a function.\n", (yyvsp[-1].decl).id);
		   (yyval.node) = 0;
		 } else {
		     Delete(SwigType_pop_function((yyvsp[-1].decl).type));
		     /* Need check for function here */
		     SwigType_push((yyvsp[-2].type),(yyvsp[-1].decl).type);
		     (yyval.node) = new_node("native");
	             Setattr((yyval.node),"name",(yyvsp[-5].id));
		     Setattr((yyval.node),"wrap:name",(yyvsp[-1].decl).id);
		     Setattr((yyval.node),"type",(yyvsp[-2].type));
		     Setattr((yyval.node),"parms",(yyvsp[-1].decl).parms);
		     Setattr((yyval.node),"decl",(yyvsp[-1].decl).type);
		 }
	         add_symbols((yyval.node));
	       }
#line 5256 "y.tab.c" /* yacc.c:1646  */
    break;

  case 73:
#line 2089 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.node) = new_node("pragma");
		 Setattr((yyval.node),"lang",(yyvsp[-3].id));
		 Setattr((yyval.node),"name",(yyvsp[-2].id));
		 Setattr((yyval.node),"value",(yyvsp[0].str));
	       }
#line 5267 "y.tab.c" /* yacc.c:1646  */
    break;

  case 74:
#line 2095 "parser.y" /* yacc.c:1646  */
    {
		(yyval.node) = new_node("pragma");
		Setattr((yyval.node),"lang",(yyvsp[-1].id));
		Setattr((yyval.node),"name",(yyvsp[0].id));
	      }
#line 5277 "y.tab.c" /* yacc.c:1646  */
    break;

  case 75:
#line 2102 "parser.y" /* yacc.c:1646  */
    { (yyval.str) = NewString((yyvsp[0].id)); }
#line 5283 "y.tab.c" /* yacc.c:1646  */
    break;

  case 76:
#line 2103 "parser.y" /* yacc.c:1646  */
    { (yyval.str) = (yyvsp[0].str); }
#line 5289 "y.tab.c" /* yacc.c:1646  */
    break;

  case 77:
#line 2106 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = (yyvsp[-1].id); }
#line 5295 "y.tab.c" /* yacc.c:1646  */
    break;

  case 78:
#line 2107 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = (char *) "swig"; }
#line 5301 "y.tab.c" /* yacc.c:1646  */
    break;

  case 79:
#line 2114 "parser.y" /* yacc.c:1646  */
    {
                SwigType *t = (yyvsp[-2].decl).type;
		Hash *kws = NewHash();
		String *fixname;
		fixname = feature_identifier_fix((yyvsp[-2].decl).id);
		Setattr(kws,"name",(yyvsp[-1].id));
		if (!Len(t)) t = 0;
		/* Special declarator check */
		if (t) {
		  if (SwigType_isfunction(t)) {
		    SwigType *decl = SwigType_pop_function(t);
		    if (SwigType_ispointer(t)) {
		      String *nname = NewStringf("*%s",fixname);
		      if ((yyvsp[-3].intvalue)) {
			Swig_name_rename_add(Namespaceprefix, nname,decl,kws,(yyvsp[-2].decl).parms);
		      } else {
			Swig_name_namewarn_add(Namespaceprefix,nname,decl,kws);
		      }
		      Delete(nname);
		    } else {
		      if ((yyvsp[-3].intvalue)) {
			Swig_name_rename_add(Namespaceprefix,(fixname),decl,kws,(yyvsp[-2].decl).parms);
		      } else {
			Swig_name_namewarn_add(Namespaceprefix,(fixname),decl,kws);
		      }
		    }
		    Delete(decl);
		  } else if (SwigType_ispointer(t)) {
		    String *nname = NewStringf("*%s",fixname);
		    if ((yyvsp[-3].intvalue)) {
		      Swig_name_rename_add(Namespaceprefix,(nname),0,kws,(yyvsp[-2].decl).parms);
		    } else {
		      Swig_name_namewarn_add(Namespaceprefix,(nname),0,kws);
		    }
		    Delete(nname);
		  }
		} else {
		  if ((yyvsp[-3].intvalue)) {
		    Swig_name_rename_add(Namespaceprefix,(fixname),0,kws,(yyvsp[-2].decl).parms);
		  } else {
		    Swig_name_namewarn_add(Namespaceprefix,(fixname),0,kws);
		  }
		}
                (yyval.node) = 0;
		scanner_clear_rename();
              }
#line 5352 "y.tab.c" /* yacc.c:1646  */
    break;

  case 80:
#line 2160 "parser.y" /* yacc.c:1646  */
    {
		String *fixname;
		Hash *kws = (yyvsp[-4].node);
		SwigType *t = (yyvsp[-2].decl).type;
		fixname = feature_identifier_fix((yyvsp[-2].decl).id);
		if (!Len(t)) t = 0;
		/* Special declarator check */
		if (t) {
		  if ((yyvsp[-1].dtype).qualifier) SwigType_push(t,(yyvsp[-1].dtype).qualifier);
		  if (SwigType_isfunction(t)) {
		    SwigType *decl = SwigType_pop_function(t);
		    if (SwigType_ispointer(t)) {
		      String *nname = NewStringf("*%s",fixname);
		      if ((yyvsp[-6].intvalue)) {
			Swig_name_rename_add(Namespaceprefix, nname,decl,kws,(yyvsp[-2].decl).parms);
		      } else {
			Swig_name_namewarn_add(Namespaceprefix,nname,decl,kws);
		      }
		      Delete(nname);
		    } else {
		      if ((yyvsp[-6].intvalue)) {
			Swig_name_rename_add(Namespaceprefix,(fixname),decl,kws,(yyvsp[-2].decl).parms);
		      } else {
			Swig_name_namewarn_add(Namespaceprefix,(fixname),decl,kws);
		      }
		    }
		    Delete(decl);
		  } else if (SwigType_ispointer(t)) {
		    String *nname = NewStringf("*%s",fixname);
		    if ((yyvsp[-6].intvalue)) {
		      Swig_name_rename_add(Namespaceprefix,(nname),0,kws,(yyvsp[-2].decl).parms);
		    } else {
		      Swig_name_namewarn_add(Namespaceprefix,(nname),0,kws);
		    }
		    Delete(nname);
		  }
		} else {
		  if ((yyvsp[-6].intvalue)) {
		    Swig_name_rename_add(Namespaceprefix,(fixname),0,kws,(yyvsp[-2].decl).parms);
		  } else {
		    Swig_name_namewarn_add(Namespaceprefix,(fixname),0,kws);
		  }
		}
                (yyval.node) = 0;
		scanner_clear_rename();
              }
#line 5403 "y.tab.c" /* yacc.c:1646  */
    break;

  case 81:
#line 2206 "parser.y" /* yacc.c:1646  */
    {
		if ((yyvsp[-5].intvalue)) {
		  Swig_name_rename_add(Namespaceprefix,(yyvsp[-1].id),0,(yyvsp[-3].node),0);
		} else {
		  Swig_name_namewarn_add(Namespaceprefix,(yyvsp[-1].id),0,(yyvsp[-3].node));
		}
		(yyval.node) = 0;
		scanner_clear_rename();
              }
#line 5417 "y.tab.c" /* yacc.c:1646  */
    break;

  case 82:
#line 2217 "parser.y" /* yacc.c:1646  */
    {
		    (yyval.intvalue) = 1;
                }
#line 5425 "y.tab.c" /* yacc.c:1646  */
    break;

  case 83:
#line 2220 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.intvalue) = 0;
                }
#line 5433 "y.tab.c" /* yacc.c:1646  */
    break;

  case 84:
#line 2247 "parser.y" /* yacc.c:1646  */
    {
                    String *val = (yyvsp[0].str) ? NewString((yyvsp[0].str)) : NewString("1");
                    new_feature((yyvsp[-4].id), val, 0, (yyvsp[-2].decl).id, (yyvsp[-2].decl).type, (yyvsp[-2].decl).parms, (yyvsp[-1].dtype).qualifier);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
#line 5444 "y.tab.c" /* yacc.c:1646  */
    break;

  case 85:
#line 2253 "parser.y" /* yacc.c:1646  */
    {
                    String *val = Len((yyvsp[-4].id)) ? NewString((yyvsp[-4].id)) : 0;
                    new_feature((yyvsp[-6].id), val, 0, (yyvsp[-2].decl).id, (yyvsp[-2].decl).type, (yyvsp[-2].decl).parms, (yyvsp[-1].dtype).qualifier);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
#line 5455 "y.tab.c" /* yacc.c:1646  */
    break;

  case 86:
#line 2259 "parser.y" /* yacc.c:1646  */
    {
                    String *val = (yyvsp[0].str) ? NewString((yyvsp[0].str)) : NewString("1");
                    new_feature((yyvsp[-5].id), val, (yyvsp[-4].node), (yyvsp[-2].decl).id, (yyvsp[-2].decl).type, (yyvsp[-2].decl).parms, (yyvsp[-1].dtype).qualifier);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
#line 5466 "y.tab.c" /* yacc.c:1646  */
    break;

  case 87:
#line 2265 "parser.y" /* yacc.c:1646  */
    {
                    String *val = Len((yyvsp[-5].id)) ? NewString((yyvsp[-5].id)) : 0;
                    new_feature((yyvsp[-7].id), val, (yyvsp[-4].node), (yyvsp[-2].decl).id, (yyvsp[-2].decl).type, (yyvsp[-2].decl).parms, (yyvsp[-1].dtype).qualifier);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
#line 5477 "y.tab.c" /* yacc.c:1646  */
    break;

  case 88:
#line 2273 "parser.y" /* yacc.c:1646  */
    {
                    String *val = (yyvsp[0].str) ? NewString((yyvsp[0].str)) : NewString("1");
                    new_feature((yyvsp[-2].id), val, 0, 0, 0, 0, 0);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
#line 5488 "y.tab.c" /* yacc.c:1646  */
    break;

  case 89:
#line 2279 "parser.y" /* yacc.c:1646  */
    {
                    String *val = Len((yyvsp[-2].id)) ? NewString((yyvsp[-2].id)) : 0;
                    new_feature((yyvsp[-4].id), val, 0, 0, 0, 0, 0);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
#line 5499 "y.tab.c" /* yacc.c:1646  */
    break;

  case 90:
#line 2285 "parser.y" /* yacc.c:1646  */
    {
                    String *val = (yyvsp[0].str) ? NewString((yyvsp[0].str)) : NewString("1");
                    new_feature((yyvsp[-3].id), val, (yyvsp[-2].node), 0, 0, 0, 0);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
#line 5510 "y.tab.c" /* yacc.c:1646  */
    break;

  case 91:
#line 2291 "parser.y" /* yacc.c:1646  */
    {
                    String *val = Len((yyvsp[-3].id)) ? NewString((yyvsp[-3].id)) : 0;
                    new_feature((yyvsp[-5].id), val, (yyvsp[-2].node), 0, 0, 0, 0);
                    (yyval.node) = 0;
                    scanner_clear_rename();
                  }
#line 5521 "y.tab.c" /* yacc.c:1646  */
    break;

  case 92:
#line 2299 "parser.y" /* yacc.c:1646  */
    { (yyval.str) = (yyvsp[0].str); }
#line 5527 "y.tab.c" /* yacc.c:1646  */
    break;

  case 93:
#line 2300 "parser.y" /* yacc.c:1646  */
    { (yyval.str) = 0; }
#line 5533 "y.tab.c" /* yacc.c:1646  */
    break;

  case 94:
#line 2301 "parser.y" /* yacc.c:1646  */
    { (yyval.str) = (yyvsp[-2].pl); }
#line 5539 "y.tab.c" /* yacc.c:1646  */
    break;

  case 95:
#line 2304 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.node) = NewHash();
		  Setattr((yyval.node),"name",(yyvsp[-2].id));
		  Setattr((yyval.node),"value",(yyvsp[0].id));
                }
#line 5549 "y.tab.c" /* yacc.c:1646  */
    break;

  case 96:
#line 2309 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.node) = NewHash();
		  Setattr((yyval.node),"name",(yyvsp[-3].id));
		  Setattr((yyval.node),"value",(yyvsp[-1].id));
                  set_nextSibling((yyval.node),(yyvsp[0].node));
                }
#line 5560 "y.tab.c" /* yacc.c:1646  */
    break;

  case 97:
#line 2319 "parser.y" /* yacc.c:1646  */
    {
                 Parm *val;
		 String *name;
		 SwigType *t;
		 if (Namespaceprefix) name = NewStringf("%s::%s", Namespaceprefix, (yyvsp[-2].decl).id);
		 else name = NewString((yyvsp[-2].decl).id);
		 val = (yyvsp[-4].pl);
		 if ((yyvsp[-2].decl).parms) {
		   Setmeta(val,"parms",(yyvsp[-2].decl).parms);
		 }
		 t = (yyvsp[-2].decl).type;
		 if (!Len(t)) t = 0;
		 if (t) {
		   if ((yyvsp[-1].dtype).qualifier) SwigType_push(t,(yyvsp[-1].dtype).qualifier);
		   if (SwigType_isfunction(t)) {
		     SwigType *decl = SwigType_pop_function(t);
		     if (SwigType_ispointer(t)) {
		       String *nname = NewStringf("*%s",name);
		       Swig_feature_set(Swig_cparse_features(), nname, decl, "feature:varargs", val, 0);
		       Delete(nname);
		     } else {
		       Swig_feature_set(Swig_cparse_features(), name, decl, "feature:varargs", val, 0);
		     }
		     Delete(decl);
		   } else if (SwigType_ispointer(t)) {
		     String *nname = NewStringf("*%s",name);
		     Swig_feature_set(Swig_cparse_features(),nname,0,"feature:varargs",val, 0);
		     Delete(nname);
		   }
		 } else {
		   Swig_feature_set(Swig_cparse_features(),name,0,"feature:varargs",val, 0);
		 }
		 Delete(name);
		 (yyval.node) = 0;
              }
#line 5600 "y.tab.c" /* yacc.c:1646  */
    break;

  case 98:
#line 2355 "parser.y" /* yacc.c:1646  */
    { (yyval.pl) = (yyvsp[0].pl); }
#line 5606 "y.tab.c" /* yacc.c:1646  */
    break;

  case 99:
#line 2356 "parser.y" /* yacc.c:1646  */
    { 
		  int i;
		  int n;
		  Parm *p;
		  n = atoi(Char((yyvsp[-2].dtype).val));
		  if (n <= 0) {
		    Swig_error(cparse_file, cparse_line,"Argument count in %%varargs must be positive.\n");
		    (yyval.pl) = 0;
		  } else {
		    String *name = Getattr((yyvsp[0].p), "name");
		    (yyval.pl) = Copy((yyvsp[0].p));
		    if (name)
		      Setattr((yyval.pl), "name", NewStringf("%s%d", name, n));
		    for (i = 1; i < n; i++) {
		      p = Copy((yyvsp[0].p));
		      name = Getattr(p, "name");
		      if (name)
		        Setattr(p, "name", NewStringf("%s%d", name, n-i));
		      set_nextSibling(p,(yyval.pl));
		      Delete((yyval.pl));
		      (yyval.pl) = p;
		    }
		  }
                }
#line 5635 "y.tab.c" /* yacc.c:1646  */
    break;

  case 100:
#line 2391 "parser.y" /* yacc.c:1646  */
    {
		   (yyval.node) = 0;
		   if ((yyvsp[-3].tmap).method) {
		     String *code = 0;
		     (yyval.node) = new_node("typemap");
		     Setattr((yyval.node),"method",(yyvsp[-3].tmap).method);
		     if ((yyvsp[-3].tmap).kwargs) {
		       ParmList *kw = (yyvsp[-3].tmap).kwargs;
                       code = remove_block(kw, (yyvsp[0].str));
		       Setattr((yyval.node),"kwargs", (yyvsp[-3].tmap).kwargs);
		     }
		     code = code ? code : NewString((yyvsp[0].str));
		     Setattr((yyval.node),"code", code);
		     Delete(code);
		     appendChild((yyval.node),(yyvsp[-1].p));
		   }
	       }
#line 5657 "y.tab.c" /* yacc.c:1646  */
    break;

  case 101:
#line 2408 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.node) = 0;
		 if ((yyvsp[-3].tmap).method) {
		   (yyval.node) = new_node("typemap");
		   Setattr((yyval.node),"method",(yyvsp[-3].tmap).method);
		   appendChild((yyval.node),(yyvsp[-1].p));
		 }
	       }
#line 5670 "y.tab.c" /* yacc.c:1646  */
    break;

  case 102:
#line 2416 "parser.y" /* yacc.c:1646  */
    {
		   (yyval.node) = 0;
		   if ((yyvsp[-5].tmap).method) {
		     (yyval.node) = new_node("typemapcopy");
		     Setattr((yyval.node),"method",(yyvsp[-5].tmap).method);
		     Setattr((yyval.node),"pattern", Getattr((yyvsp[-1].p),"pattern"));
		     appendChild((yyval.node),(yyvsp[-3].p));
		   }
	       }
#line 5684 "y.tab.c" /* yacc.c:1646  */
    break;

  case 103:
#line 2429 "parser.y" /* yacc.c:1646  */
    {
		 Hash *p;
		 String *name;
		 p = nextSibling((yyvsp[0].node));
		 if (p && (!Getattr(p,"value"))) {
 		   /* this is the deprecated two argument typemap form */
 		   Swig_warning(WARN_DEPRECATED_TYPEMAP_LANG,cparse_file, cparse_line,
				"Specifying the language name in %%typemap is deprecated - use #ifdef SWIG<LANG> instead.\n");
		   /* two argument typemap form */
		   name = Getattr((yyvsp[0].node),"name");
		   if (!name || (Strcmp(name,typemap_lang))) {
		     (yyval.tmap).method = 0;
		     (yyval.tmap).kwargs = 0;
		   } else {
		     (yyval.tmap).method = Getattr(p,"name");
		     (yyval.tmap).kwargs = nextSibling(p);
		   }
		 } else {
		   /* one-argument typemap-form */
		   (yyval.tmap).method = Getattr((yyvsp[0].node),"name");
		   (yyval.tmap).kwargs = p;
		 }
                }
#line 5712 "y.tab.c" /* yacc.c:1646  */
    break;

  case 104:
#line 2454 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.p) = (yyvsp[-1].p);
		 set_nextSibling((yyval.p),(yyvsp[0].p));
		}
#line 5721 "y.tab.c" /* yacc.c:1646  */
    break;

  case 105:
#line 2460 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.p) = (yyvsp[-1].p);
		 set_nextSibling((yyval.p),(yyvsp[0].p));
                }
#line 5730 "y.tab.c" /* yacc.c:1646  */
    break;

  case 106:
#line 2464 "parser.y" /* yacc.c:1646  */
    { (yyval.p) = 0;}
#line 5736 "y.tab.c" /* yacc.c:1646  */
    break;

  case 107:
#line 2467 "parser.y" /* yacc.c:1646  */
    {
                  Parm *parm;
		  SwigType_push((yyvsp[-1].type),(yyvsp[0].decl).type);
		  (yyval.p) = new_node("typemapitem");
		  parm = NewParmWithoutFileLineInfo((yyvsp[-1].type),(yyvsp[0].decl).id);
		  Setattr((yyval.p),"pattern",parm);
		  Setattr((yyval.p),"parms", (yyvsp[0].decl).parms);
		  Delete(parm);
		  /*		  $$ = NewParmWithoutFileLineInfo($1,$2.id);
				  Setattr($$,"parms",$2.parms); */
                }
#line 5752 "y.tab.c" /* yacc.c:1646  */
    break;

  case 108:
#line 2478 "parser.y" /* yacc.c:1646  */
    {
                  (yyval.p) = new_node("typemapitem");
		  Setattr((yyval.p),"pattern",(yyvsp[-1].pl));
		  /*		  Setattr($$,"multitype",$2); */
               }
#line 5762 "y.tab.c" /* yacc.c:1646  */
    break;

  case 109:
#line 2483 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.p) = new_node("typemapitem");
		 Setattr((yyval.p),"pattern", (yyvsp[-4].pl));
		 /*                 Setattr($$,"multitype",$2); */
		 Setattr((yyval.p),"parms",(yyvsp[-1].pl));
               }
#line 5773 "y.tab.c" /* yacc.c:1646  */
    break;

  case 110:
#line 2496 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.node) = new_node("types");
		   Setattr((yyval.node),"parms",(yyvsp[-2].pl));
                   if ((yyvsp[0].str))
		     Setattr((yyval.node),"convcode",NewString((yyvsp[0].str)));
               }
#line 5784 "y.tab.c" /* yacc.c:1646  */
    break;

  case 111:
#line 2508 "parser.y" /* yacc.c:1646  */
    {
                  Parm *p, *tp;
		  Node *n;
		  Node *outer_class = currentOuterClass;
		  Symtab *tscope = 0;
		  int     specialized = 0;
		  int     variadic = 0;

		  (yyval.node) = 0;

		  tscope = Swig_symbol_current();          /* Get the current scope */

		  /* If the class name is qualified, we need to create or lookup namespace entries */
		  if (!inclass) {
		    (yyvsp[-4].str) = resolve_create_node_scope((yyvsp[-4].str));
		  }
		  if (nscope_inner && Strcmp(nodeType(nscope_inner), "class") == 0) {
		    outer_class	= nscope_inner;
		  }

		  /*
		    We use the new namespace entry 'nscope' only to
		    emit the template node. The template parameters are
		    resolved in the current 'tscope'.

		    This is closer to the C++ (typedef) behavior.
		  */
		  n = Swig_cparse_template_locate((yyvsp[-4].str),(yyvsp[-2].p),tscope);

		  /* Patch the argument types to respect namespaces */
		  p = (yyvsp[-2].p);
		  while (p) {
		    SwigType *value = Getattr(p,"value");
		    if (!value) {
		      SwigType *ty = Getattr(p,"type");
		      if (ty) {
			SwigType *rty = 0;
			int reduce = template_reduce;
			if (reduce || !SwigType_ispointer(ty)) {
			  rty = Swig_symbol_typedef_reduce(ty,tscope);
			  if (!reduce) reduce = SwigType_ispointer(rty);
			}
			ty = reduce ? Swig_symbol_type_qualify(rty,tscope) : Swig_symbol_type_qualify(ty,tscope);
			Setattr(p,"type",ty);
			Delete(ty);
			Delete(rty);
		      }
		    } else {
		      value = Swig_symbol_type_qualify(value,tscope);
		      Setattr(p,"value",value);
		      Delete(value);
		    }

		    p = nextSibling(p);
		  }

		  /* Look for the template */
		  {
                    Node *nn = n;
                    Node *linklistend = 0;
                    while (nn) {
                      Node *templnode = 0;
                      if (Strcmp(nodeType(nn),"template") == 0) {
                        int nnisclass = (Strcmp(Getattr(nn,"templatetype"),"class") == 0); /* if not a templated class it is a templated function */
                        Parm *tparms = Getattr(nn,"templateparms");
                        if (!tparms) {
                          specialized = 1;
                        } else if (Getattr(tparms,"variadic") && strncmp(Char(Getattr(tparms,"variadic")), "1", 1)==0) {
                          variadic = 1;
                        }
                        if (nnisclass && !variadic && !specialized && (ParmList_len((yyvsp[-2].p)) > ParmList_len(tparms))) {
                          Swig_error(cparse_file, cparse_line, "Too many template parameters. Maximum of %d.\n", ParmList_len(tparms));
                        } else if (nnisclass && !specialized && ((ParmList_len((yyvsp[-2].p)) < (ParmList_numrequired(tparms) - (variadic?1:0))))) { /* Variadic parameter is optional */
                          Swig_error(cparse_file, cparse_line, "Not enough template parameters specified. %d required.\n", (ParmList_numrequired(tparms)-(variadic?1:0)) );
                        } else if (!nnisclass && ((ParmList_len((yyvsp[-2].p)) != ParmList_len(tparms)))) {
                          /* must be an overloaded templated method - ignore it as it is overloaded with a different number of template parameters */
                          nn = Getattr(nn,"sym:nextSibling"); /* repeat for overloaded templated functions */
                          continue;
                        } else {
			  String *tname = Copy((yyvsp[-4].str));
                          int def_supplied = 0;
                          /* Expand the template */
			  Node *templ = Swig_symbol_clookup((yyvsp[-4].str),0);
			  Parm *targs = templ ? Getattr(templ,"templateparms") : 0;

                          ParmList *temparms;
                          if (specialized) temparms = CopyParmList((yyvsp[-2].p));
                          else temparms = CopyParmList(tparms);

                          /* Create typedef's and arguments */
                          p = (yyvsp[-2].p);
                          tp = temparms;
                          if (!p && ParmList_len(p) != ParmList_len(temparms)) {
                            /* we have no template parameters supplied in %template for a template that has default args*/
                            p = tp;
                            def_supplied = 1;
                          }

                          while (p) {
                            String *value = Getattr(p,"value");
                            if (def_supplied) {
                              Setattr(p,"default","1");
                            }
                            if (value) {
                              Setattr(tp,"value",value);
                            } else {
                              SwigType *ty = Getattr(p,"type");
                              if (ty) {
                                Setattr(tp,"type",ty);
                              }
                              Delattr(tp,"value");
                            }
			    /* fix default arg values */
			    if (targs) {
			      Parm *pi = temparms;
			      Parm *ti = targs;
			      String *tv = Getattr(tp,"value");
			      if (!tv) tv = Getattr(tp,"type");
			      while(pi != tp && ti && pi) {
				String *name = Getattr(ti,"name");
				String *value = Getattr(pi,"value");
				if (!value) value = Getattr(pi,"type");
				Replaceid(tv, name, value);
				pi = nextSibling(pi);
				ti = nextSibling(ti);
			      }
			    }
                            p = nextSibling(p);
                            tp = nextSibling(tp);
                            if (!p && tp) {
                              p = tp;
                              def_supplied = 1;
                            } else if (p && !tp) { /* Variadic template - tp < p */
			      SWIG_WARN_NODE_BEGIN(nn);
                              Swig_warning(WARN_CPP11_VARIADIC_TEMPLATE,cparse_file, cparse_line,"Only the first variadic template argument is currently supported.\n");
			      SWIG_WARN_NODE_END(nn);
                              break;
                            }
                          }

                          templnode = copy_node(nn);
			  update_nested_classes(templnode); /* update classes nested withing template */
                          /* We need to set the node name based on name used to instantiate */
                          Setattr(templnode,"name",tname);
			  Delete(tname);
                          if (!specialized) {
                            Delattr(templnode,"sym:typename");
                          } else {
                            Setattr(templnode,"sym:typename","1");
                          }
			  /* for now, nested %template is allowed only in the same scope as the template declaration */
                          if ((yyvsp[-6].id) && !(nnisclass && ((outer_class && (outer_class != Getattr(nn, "nested:outer")))
			    ||(extendmode && current_class && (current_class != Getattr(nn, "nested:outer")))))) {
			    /*
			       Comment this out for 1.3.28. We need to
			       re-enable it later but first we need to
			       move %ignore from using %rename to use
			       %feature(ignore).

			       String *symname = Swig_name_make(templnode,0,$3,0,0);
			    */
			    String *symname = (yyvsp[-6].id);
                            Swig_cparse_template_expand(templnode,symname,temparms,tscope);
                            Setattr(templnode,"sym:name",symname);
                          } else {
                            static int cnt = 0;
                            String *nname = NewStringf("__dummy_%d__", cnt++);
                            Swig_cparse_template_expand(templnode,nname,temparms,tscope);
                            Setattr(templnode,"sym:name",nname);
			    Delete(nname);
                            Setattr(templnode,"feature:onlychildren", "typemap,typemapitem,typemapcopy,typedef,types,fragment");
			    if ((yyvsp[-6].id)) {
			      Swig_warning(WARN_PARSE_NESTED_TEMPLATE, cparse_file, cparse_line, "Named nested template instantiations not supported. Processing as if no name was given to %%template().\n");
			    }
                          }
                          Delattr(templnode,"templatetype");
                          Setattr(templnode,"template",nn);
                          Setfile(templnode,cparse_file);
                          Setline(templnode,cparse_line);
                          Delete(temparms);
			  if (outer_class && nnisclass) {
			    SetFlag(templnode, "nested");
			    Setattr(templnode, "nested:outer", outer_class);
			  }
                          add_symbols_copy(templnode);

                          if (Strcmp(nodeType(templnode),"class") == 0) {

                            /* Identify pure abstract methods */
                            Setattr(templnode,"abstracts", pure_abstracts(firstChild(templnode)));

                            /* Set up inheritance in symbol table */
                            {
                              Symtab  *csyms;
                              List *baselist = Getattr(templnode,"baselist");
                              csyms = Swig_symbol_current();
                              Swig_symbol_setscope(Getattr(templnode,"symtab"));
                              if (baselist) {
                                List *bases = Swig_make_inherit_list(Getattr(templnode,"name"),baselist, Namespaceprefix);
                                if (bases) {
                                  Iterator s;
                                  for (s = First(bases); s.item; s = Next(s)) {
                                    Symtab *st = Getattr(s.item,"symtab");
                                    if (st) {
				      Setfile(st,Getfile(s.item));
				      Setline(st,Getline(s.item));
                                      Swig_symbol_inherit(st);
                                    }
                                  }
				  Delete(bases);
                                }
                              }
                              Swig_symbol_setscope(csyms);
                            }

                            /* Merge in %extend methods for this class */

			    /* !!! This may be broken.  We may have to add the
			       %extend methods at the beginning of the class */
                            {
                              String *stmp = 0;
                              String *clsname;
                              Node *am;
                              if (Namespaceprefix) {
                                clsname = stmp = NewStringf("%s::%s", Namespaceprefix, Getattr(templnode,"name"));
                              } else {
                                clsname = Getattr(templnode,"name");
                              }
                              am = Getattr(Swig_extend_hash(),clsname);
                              if (am) {
                                Symtab *st = Swig_symbol_current();
                                Swig_symbol_setscope(Getattr(templnode,"symtab"));
                                /*			    Printf(stdout,"%s: %s %p %p\n", Getattr(templnode,"name"), clsname, Swig_symbol_current(), Getattr(templnode,"symtab")); */
                                Swig_extend_merge(templnode,am);
                                Swig_symbol_setscope(st);
				Swig_extend_append_previous(templnode,am);
                                Delattr(Swig_extend_hash(),clsname);
                              }
			      if (stmp) Delete(stmp);
                            }

                            /* Add to classes hash */
			    if (!classes)
			      classes = NewHash();

			    if (Namespaceprefix) {
			      String *temp = NewStringf("%s::%s", Namespaceprefix, Getattr(templnode,"name"));
			      Setattr(classes,temp,templnode);
			      Delete(temp);
			    } else {
			      String *qs = Swig_symbol_qualifiedscopename(templnode);
			      Setattr(classes, qs,templnode);
			      Delete(qs);
			    }
                          }
                        }

                        /* all the overloaded templated functions are added into a linked list */
                        if (nscope_inner) {
                          /* non-global namespace */
                          if (templnode) {
                            appendChild(nscope_inner,templnode);
			    Delete(templnode);
                            if (nscope) (yyval.node) = nscope;
                          }
                        } else {
                          /* global namespace */
                          if (!linklistend) {
                            (yyval.node) = templnode;
                          } else {
                            set_nextSibling(linklistend,templnode);
			    Delete(templnode);
                          }
                          linklistend = templnode;
                        }
                      }
                      nn = Getattr(nn,"sym:nextSibling"); /* repeat for overloaded templated functions. If a templated class there will never be a sibling. */
                    }
		  }
	          Swig_symbol_setscope(tscope);
		  Delete(Namespaceprefix);
		  Namespaceprefix = Swig_symbol_qualifiedscopename(0);
                }
#line 6072 "y.tab.c" /* yacc.c:1646  */
    break;

  case 112:
#line 2798 "parser.y" /* yacc.c:1646  */
    {
		  Swig_warning(0,cparse_file, cparse_line,"%s\n", (yyvsp[0].id));
		  (yyval.node) = 0;
               }
#line 6081 "y.tab.c" /* yacc.c:1646  */
    break;

  case 113:
#line 2808 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.node) = (yyvsp[0].node); 
                    if ((yyval.node)) {
   		      add_symbols((yyval.node));
                      default_arguments((yyval.node));
   	            }
                }
#line 6093 "y.tab.c" /* yacc.c:1646  */
    break;

  case 114:
#line 2815 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 6099 "y.tab.c" /* yacc.c:1646  */
    break;

  case 115:
#line 2816 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 6105 "y.tab.c" /* yacc.c:1646  */
    break;

  case 116:
#line 2820 "parser.y" /* yacc.c:1646  */
    {
		  if (Strcmp((yyvsp[-1].id),"C") == 0) {
		    cparse_externc = 1;
		  }
		}
#line 6115 "y.tab.c" /* yacc.c:1646  */
    break;

  case 117:
#line 2824 "parser.y" /* yacc.c:1646  */
    {
		  cparse_externc = 0;
		  if (Strcmp((yyvsp[-4].id),"C") == 0) {
		    Node *n = firstChild((yyvsp[-1].node));
		    (yyval.node) = new_node("extern");
		    Setattr((yyval.node),"name",(yyvsp[-4].id));
		    appendChild((yyval.node),n);
		    while (n) {
		      SwigType *decl = Getattr(n,"decl");
		      if (SwigType_isfunction(decl) && !Equal(Getattr(n, "storage"), "typedef")) {
			Setattr(n,"storage","externc");
		      }
		      n = nextSibling(n);
		    }
		  } else {
		     Swig_warning(WARN_PARSE_UNDEFINED_EXTERN,cparse_file, cparse_line,"Unrecognized extern type \"%s\".\n", (yyvsp[-4].id));
		    (yyval.node) = new_node("extern");
		    Setattr((yyval.node),"name",(yyvsp[-4].id));
		    appendChild((yyval.node),firstChild((yyvsp[-1].node)));
		  }
                }
#line 6141 "y.tab.c" /* yacc.c:1646  */
    break;

  case 118:
#line 2845 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.node) = (yyvsp[0].node);
		  SWIG_WARN_NODE_BEGIN((yyval.node));
		  Swig_warning(WARN_CPP11_LAMBDA, cparse_file, cparse_line, "Lambda expressions and closures are not fully supported yet.\n");
		  SWIG_WARN_NODE_END((yyval.node));
		}
#line 6152 "y.tab.c" /* yacc.c:1646  */
    break;

  case 119:
#line 2851 "parser.y" /* yacc.c:1646  */
    {
		  skip_decl();
		  (yyval.node) = new_node("using");
		  Setattr((yyval.node),"name",(yyvsp[-1].str));
		  add_symbols((yyval.node));
		  SWIG_WARN_NODE_BEGIN((yyval.node));
		  Swig_warning(WARN_CPP11_ALIAS_DECLARATION, cparse_file, cparse_line, "The 'using' keyword in type aliasing is not fully supported yet.\n");
		  SWIG_WARN_NODE_END((yyval.node));

		  (yyval.node) = 0; /* TODO - ignored for now */
		}
#line 6168 "y.tab.c" /* yacc.c:1646  */
    break;

  case 120:
#line 2862 "parser.y" /* yacc.c:1646  */
    {
		  skip_decl();
		  (yyval.node) = new_node("using");
		  Setattr((yyval.node),"uname",(yyvsp[0].id));
		  Setattr((yyval.node),"name",(yyvsp[-2].str));
		  add_symbols((yyval.node));
		  SWIG_WARN_NODE_BEGIN((yyval.node));
		  Swig_warning(WARN_CPP11_ALIAS_TEMPLATE, cparse_file, cparse_line, "The 'using' keyword in template aliasing is not fully supported yet.\n");
		  SWIG_WARN_NODE_END((yyval.node));
		}
#line 6183 "y.tab.c" /* yacc.c:1646  */
    break;

  case 121:
#line 2878 "parser.y" /* yacc.c:1646  */
    {
              (yyval.node) = new_node("cdecl");
	      if ((yyvsp[-1].dtype).qualifier) SwigType_push((yyvsp[-2].decl).type,(yyvsp[-1].dtype).qualifier);
	      Setattr((yyval.node),"type",(yyvsp[-3].type));
	      Setattr((yyval.node),"storage",(yyvsp[-4].id));
	      Setattr((yyval.node),"name",(yyvsp[-2].decl).id);
	      Setattr((yyval.node),"decl",(yyvsp[-2].decl).type);
	      Setattr((yyval.node),"parms",(yyvsp[-2].decl).parms);
	      Setattr((yyval.node),"value",(yyvsp[-1].dtype).val);
	      Setattr((yyval.node),"throws",(yyvsp[-1].dtype).throws);
	      Setattr((yyval.node),"throw",(yyvsp[-1].dtype).throwf);
	      Setattr((yyval.node),"noexcept",(yyvsp[-1].dtype).nexcept);
	      if (!(yyvsp[0].node)) {
		if (Len(scanner_ccode)) {
		  String *code = Copy(scanner_ccode);
		  Setattr((yyval.node),"code",code);
		  Delete(code);
		}
	      } else {
		Node *n = (yyvsp[0].node);
		/* Inherit attributes */
		while (n) {
		  String *type = Copy((yyvsp[-3].type));
		  Setattr(n,"type",type);
		  Setattr(n,"storage",(yyvsp[-4].id));
		  n = nextSibling(n);
		  Delete(type);
		}
	      }
	      if ((yyvsp[-1].dtype).bitfield) {
		Setattr((yyval.node),"bitfield", (yyvsp[-1].dtype).bitfield);
	      }

	      /* Look for "::" declarations (ignored) */
	      if (Strstr((yyvsp[-2].decl).id,"::")) {
                /* This is a special case. If the scope name of the declaration exactly
                   matches that of the declaration, then we will allow it. Otherwise, delete. */
                String *p = Swig_scopename_prefix((yyvsp[-2].decl).id);
		if (p) {
		  if ((Namespaceprefix && Strcmp(p,Namespaceprefix) == 0) ||
		      (inclass && Strcmp(p,Classprefix) == 0)) {
		    String *lstr = Swig_scopename_last((yyvsp[-2].decl).id);
		    Setattr((yyval.node),"name",lstr);
		    Delete(lstr);
		    set_nextSibling((yyval.node),(yyvsp[0].node));
		  } else {
		    Delete((yyval.node));
		    (yyval.node) = (yyvsp[0].node);
		  }
		  Delete(p);
		} else {
		  Delete((yyval.node));
		  (yyval.node) = (yyvsp[0].node);
		}
	      } else {
		set_nextSibling((yyval.node),(yyvsp[0].node));
	      }
           }
#line 6246 "y.tab.c" /* yacc.c:1646  */
    break;

  case 122:
#line 2938 "parser.y" /* yacc.c:1646  */
    {
              (yyval.node) = new_node("cdecl");
	      if ((yyvsp[-1].dtype).qualifier) SwigType_push((yyvsp[-4].decl).type,(yyvsp[-1].dtype).qualifier);
	      Setattr((yyval.node),"type",(yyvsp[-2].node));
	      Setattr((yyval.node),"storage",(yyvsp[-6].id));
	      Setattr((yyval.node),"name",(yyvsp[-4].decl).id);
	      Setattr((yyval.node),"decl",(yyvsp[-4].decl).type);
	      Setattr((yyval.node),"parms",(yyvsp[-4].decl).parms);
	      Setattr((yyval.node),"value",(yyvsp[-1].dtype).val);
	      Setattr((yyval.node),"throws",(yyvsp[-1].dtype).throws);
	      Setattr((yyval.node),"throw",(yyvsp[-1].dtype).throwf);
	      Setattr((yyval.node),"noexcept",(yyvsp[-1].dtype).nexcept);
	      if (!(yyvsp[0].node)) {
		if (Len(scanner_ccode)) {
		  String *code = Copy(scanner_ccode);
		  Setattr((yyval.node),"code",code);
		  Delete(code);
		}
	      } else {
		Node *n = (yyvsp[0].node);
		while (n) {
		  String *type = Copy((yyvsp[-2].node));
		  Setattr(n,"type",type);
		  Setattr(n,"storage",(yyvsp[-6].id));
		  n = nextSibling(n);
		  Delete(type);
		}
	      }
	      if ((yyvsp[-1].dtype).bitfield) {
		Setattr((yyval.node),"bitfield", (yyvsp[-1].dtype).bitfield);
	      }

	      if (Strstr((yyvsp[-4].decl).id,"::")) {
                String *p = Swig_scopename_prefix((yyvsp[-4].decl).id);
		if (p) {
		  if ((Namespaceprefix && Strcmp(p,Namespaceprefix) == 0) ||
		      (inclass && Strcmp(p,Classprefix) == 0)) {
		    String *lstr = Swig_scopename_last((yyvsp[-4].decl).id);
		    Setattr((yyval.node),"name",lstr);
		    Delete(lstr);
		    set_nextSibling((yyval.node),(yyvsp[0].node));
		  } else {
		    Delete((yyval.node));
		    (yyval.node) = (yyvsp[0].node);
		  }
		  Delete(p);
		} else {
		  Delete((yyval.node));
		  (yyval.node) = (yyvsp[0].node);
		}
	      } else {
		set_nextSibling((yyval.node),(yyvsp[0].node));
	      }
           }
#line 6305 "y.tab.c" /* yacc.c:1646  */
    break;

  case 123:
#line 2996 "parser.y" /* yacc.c:1646  */
    { 
                   (yyval.node) = 0;
                   Clear(scanner_ccode); 
               }
#line 6314 "y.tab.c" /* yacc.c:1646  */
    break;

  case 124:
#line 3000 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.node) = new_node("cdecl");
		 if ((yyvsp[-1].dtype).qualifier) SwigType_push((yyvsp[-2].decl).type,(yyvsp[-1].dtype).qualifier);
		 Setattr((yyval.node),"name",(yyvsp[-2].decl).id);
		 Setattr((yyval.node),"decl",(yyvsp[-2].decl).type);
		 Setattr((yyval.node),"parms",(yyvsp[-2].decl).parms);
		 Setattr((yyval.node),"value",(yyvsp[-1].dtype).val);
		 Setattr((yyval.node),"throws",(yyvsp[-1].dtype).throws);
		 Setattr((yyval.node),"throw",(yyvsp[-1].dtype).throwf);
		 Setattr((yyval.node),"noexcept",(yyvsp[-1].dtype).nexcept);
		 if ((yyvsp[-1].dtype).bitfield) {
		   Setattr((yyval.node),"bitfield", (yyvsp[-1].dtype).bitfield);
		 }
		 if (!(yyvsp[0].node)) {
		   if (Len(scanner_ccode)) {
		     String *code = Copy(scanner_ccode);
		     Setattr((yyval.node),"code",code);
		     Delete(code);
		   }
		 } else {
		   set_nextSibling((yyval.node),(yyvsp[0].node));
		 }
	       }
#line 6342 "y.tab.c" /* yacc.c:1646  */
    break;

  case 125:
#line 3023 "parser.y" /* yacc.c:1646  */
    { 
                   skip_balanced('{','}');
                   (yyval.node) = 0;
               }
#line 6351 "y.tab.c" /* yacc.c:1646  */
    break;

  case 126:
#line 3027 "parser.y" /* yacc.c:1646  */
    {
		   (yyval.node) = 0;
		   if (yychar == RPAREN) {
		       Swig_error(cparse_file, cparse_line, "Unexpected ')'.\n");
		   } else {
		       Swig_error(cparse_file, cparse_line, "Syntax error - possibly a missing semicolon.\n");
		   }
		   exit(1);
               }
#line 6365 "y.tab.c" /* yacc.c:1646  */
    break;

  case 127:
#line 3038 "parser.y" /* yacc.c:1646  */
    { 
                   (yyval.dtype) = (yyvsp[0].dtype); 
                   (yyval.dtype).qualifier = 0;
		   (yyval.dtype).throws = 0;
		   (yyval.dtype).throwf = 0;
		   (yyval.dtype).nexcept = 0;
              }
#line 6377 "y.tab.c" /* yacc.c:1646  */
    break;

  case 128:
#line 3045 "parser.y" /* yacc.c:1646  */
    { 
                   (yyval.dtype) = (yyvsp[0].dtype); 
		   (yyval.dtype).qualifier = (yyvsp[-1].str);
		   (yyval.dtype).throws = 0;
		   (yyval.dtype).throwf = 0;
		   (yyval.dtype).nexcept = 0;
	      }
#line 6389 "y.tab.c" /* yacc.c:1646  */
    break;

  case 129:
#line 3052 "parser.y" /* yacc.c:1646  */
    { 
		   (yyval.dtype) = (yyvsp[0].dtype); 
                   (yyval.dtype).qualifier = 0;
		   (yyval.dtype).throws = (yyvsp[-1].dtype).throws;
		   (yyval.dtype).throwf = (yyvsp[-1].dtype).throwf;
		   (yyval.dtype).nexcept = (yyvsp[-1].dtype).nexcept;
              }
#line 6401 "y.tab.c" /* yacc.c:1646  */
    break;

  case 130:
#line 3059 "parser.y" /* yacc.c:1646  */
    { 
                   (yyval.dtype) = (yyvsp[0].dtype); 
                   (yyval.dtype).qualifier = (yyvsp[-2].str);
		   (yyval.dtype).throws = (yyvsp[-1].dtype).throws;
		   (yyval.dtype).throwf = (yyvsp[-1].dtype).throwf;
		   (yyval.dtype).nexcept = (yyvsp[-1].dtype).nexcept;
              }
#line 6413 "y.tab.c" /* yacc.c:1646  */
    break;

  case 131:
#line 3068 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].type); }
#line 6419 "y.tab.c" /* yacc.c:1646  */
    break;

  case 132:
#line 3069 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].type); }
#line 6425 "y.tab.c" /* yacc.c:1646  */
    break;

  case 133:
#line 3070 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].type); }
#line 6431 "y.tab.c" /* yacc.c:1646  */
    break;

  case 134:
#line 3071 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = NewStringf("%s%s",(yyvsp[-1].type),(yyvsp[0].id)); }
#line 6437 "y.tab.c" /* yacc.c:1646  */
    break;

  case 135:
#line 3072 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].type); }
#line 6443 "y.tab.c" /* yacc.c:1646  */
    break;

  case 136:
#line 3073 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].str); }
#line 6449 "y.tab.c" /* yacc.c:1646  */
    break;

  case 137:
#line 3074 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].type); }
#line 6455 "y.tab.c" /* yacc.c:1646  */
    break;

  case 138:
#line 3085 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.node) = new_node("lambda");
		  Setattr((yyval.node),"name",(yyvsp[-8].str));
		  add_symbols((yyval.node));
	        }
#line 6465 "y.tab.c" /* yacc.c:1646  */
    break;

  case 139:
#line 3090 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.node) = new_node("lambda");
		  Setattr((yyval.node),"name",(yyvsp[-10].str));
		  add_symbols((yyval.node));
		}
#line 6475 "y.tab.c" /* yacc.c:1646  */
    break;

  case 140:
#line 3095 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.node) = new_node("lambda");
		  Setattr((yyval.node),"name",(yyvsp[-4].str));
		  add_symbols((yyval.node));
		}
#line 6485 "y.tab.c" /* yacc.c:1646  */
    break;

  case 141:
#line 3102 "parser.y" /* yacc.c:1646  */
    {
		  skip_balanced('[',']');
		  (yyval.node) = 0;
	        }
#line 6494 "y.tab.c" /* yacc.c:1646  */
    break;

  case 142:
#line 3108 "parser.y" /* yacc.c:1646  */
    {
		  skip_balanced('{','}');
		  (yyval.node) = 0;
		}
#line 6503 "y.tab.c" /* yacc.c:1646  */
    break;

  case 143:
#line 3113 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.pl) = 0;
		}
#line 6511 "y.tab.c" /* yacc.c:1646  */
    break;

  case 144:
#line 3116 "parser.y" /* yacc.c:1646  */
    {
		  skip_balanced('(',')');
		}
#line 6519 "y.tab.c" /* yacc.c:1646  */
    break;

  case 145:
#line 3118 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.pl) = 0;
		}
#line 6527 "y.tab.c" /* yacc.c:1646  */
    break;

  case 146:
#line 3129 "parser.y" /* yacc.c:1646  */
    {
		   (yyval.node) = (char *)"enum";
	      }
#line 6535 "y.tab.c" /* yacc.c:1646  */
    break;

  case 147:
#line 3132 "parser.y" /* yacc.c:1646  */
    {
		   (yyval.node) = (char *)"enum class";
	      }
#line 6543 "y.tab.c" /* yacc.c:1646  */
    break;

  case 148:
#line 3135 "parser.y" /* yacc.c:1646  */
    {
		   (yyval.node) = (char *)"enum struct";
	      }
#line 6551 "y.tab.c" /* yacc.c:1646  */
    break;

  case 149:
#line 3144 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.node) = (yyvsp[0].type);
              }
#line 6559 "y.tab.c" /* yacc.c:1646  */
    break;

  case 150:
#line 3147 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = 0; }
#line 6565 "y.tab.c" /* yacc.c:1646  */
    break;

  case 151:
#line 3154 "parser.y" /* yacc.c:1646  */
    {
		   SwigType *ty = 0;
		   int scopedenum = (yyvsp[-2].id) && !Equal((yyvsp[-3].node), "enum");
		   (yyval.node) = new_node("enumforward");
		   ty = NewStringf("enum %s", (yyvsp[-2].id));
		   Setattr((yyval.node),"enumkey",(yyvsp[-3].node));
		   if (scopedenum)
		     SetFlag((yyval.node), "scopedenum");
		   Setattr((yyval.node),"name",(yyvsp[-2].id));
		   Setattr((yyval.node),"inherit",(yyvsp[-1].node));
		   Setattr((yyval.node),"type",ty);
		   Setattr((yyval.node),"sym:weak", "1");
		   add_symbols((yyval.node));
	      }
#line 6584 "y.tab.c" /* yacc.c:1646  */
    break;

  case 152:
#line 3176 "parser.y" /* yacc.c:1646  */
    {
		  SwigType *ty = 0;
		  int scopedenum = (yyvsp[-5].id) && !Equal((yyvsp[-6].node), "enum");
                  (yyval.node) = new_node("enum");
		  ty = NewStringf("enum %s", (yyvsp[-5].id));
		  Setattr((yyval.node),"enumkey",(yyvsp[-6].node));
		  if (scopedenum)
		    SetFlag((yyval.node), "scopedenum");
		  Setattr((yyval.node),"name",(yyvsp[-5].id));
		  Setattr((yyval.node),"inherit",(yyvsp[-4].node));
		  Setattr((yyval.node),"type",ty);
		  appendChild((yyval.node),(yyvsp[-2].node));
		  add_symbols((yyval.node));      /* Add to tag space */

		  if (scopedenum) {
		    Swig_symbol_newscope();
		    Swig_symbol_setscopename((yyvsp[-5].id));
		    Delete(Namespaceprefix);
		    Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		  }

		  add_symbols((yyvsp[-2].node));      /* Add enum values to appropriate enum or enum class scope */

		  if (scopedenum) {
		    Setattr((yyval.node),"symtab", Swig_symbol_popscope());
		    Delete(Namespaceprefix);
		    Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		  }
               }
#line 6618 "y.tab.c" /* yacc.c:1646  */
    break;

  case 153:
#line 3205 "parser.y" /* yacc.c:1646  */
    {
		 Node *n;
		 SwigType *ty = 0;
		 String   *unnamed = 0;
		 int       unnamedinstance = 0;
		 int scopedenum = (yyvsp[-7].id) && !Equal((yyvsp[-8].node), "enum");

		 (yyval.node) = new_node("enum");
		 Setattr((yyval.node),"enumkey",(yyvsp[-8].node));
		 if (scopedenum)
		   SetFlag((yyval.node), "scopedenum");
		 Setattr((yyval.node),"inherit",(yyvsp[-6].node));
		 if ((yyvsp[-7].id)) {
		   Setattr((yyval.node),"name",(yyvsp[-7].id));
		   ty = NewStringf("enum %s", (yyvsp[-7].id));
		 } else if ((yyvsp[-2].decl).id) {
		   unnamed = make_unnamed();
		   ty = NewStringf("enum %s", unnamed);
		   Setattr((yyval.node),"unnamed",unnamed);
                   /* name is not set for unnamed enum instances, e.g. enum { foo } Instance; */
		   if ((yyvsp[-9].id) && Cmp((yyvsp[-9].id),"typedef") == 0) {
		     Setattr((yyval.node),"name",(yyvsp[-2].decl).id);
                   } else {
                     unnamedinstance = 1;
                   }
		   Setattr((yyval.node),"storage",(yyvsp[-9].id));
		 }
		 if ((yyvsp[-2].decl).id && Cmp((yyvsp[-9].id),"typedef") == 0) {
		   Setattr((yyval.node),"tdname",(yyvsp[-2].decl).id);
                   Setattr((yyval.node),"allows_typedef","1");
                 }
		 appendChild((yyval.node),(yyvsp[-4].node));
		 n = new_node("cdecl");
		 Setattr(n,"type",ty);
		 Setattr(n,"name",(yyvsp[-2].decl).id);
		 Setattr(n,"storage",(yyvsp[-9].id));
		 Setattr(n,"decl",(yyvsp[-2].decl).type);
		 Setattr(n,"parms",(yyvsp[-2].decl).parms);
		 Setattr(n,"unnamed",unnamed);

                 if (unnamedinstance) {
		   SwigType *cty = NewString("enum ");
		   Setattr((yyval.node),"type",cty);
		   SetFlag((yyval.node),"unnamedinstance");
		   SetFlag(n,"unnamedinstance");
		   Delete(cty);
                 }
		 if ((yyvsp[0].node)) {
		   Node *p = (yyvsp[0].node);
		   set_nextSibling(n,p);
		   while (p) {
		     SwigType *cty = Copy(ty);
		     Setattr(p,"type",cty);
		     Setattr(p,"unnamed",unnamed);
		     Setattr(p,"storage",(yyvsp[-9].id));
		     Delete(cty);
		     p = nextSibling(p);
		   }
		 } else {
		   if (Len(scanner_ccode)) {
		     String *code = Copy(scanner_ccode);
		     Setattr(n,"code",code);
		     Delete(code);
		   }
		 }

                 /* Ensure that typedef enum ABC {foo} XYZ; uses XYZ for sym:name, like structs.
                  * Note that class_rename/yyrename are bit of a mess so used this simple approach to change the name. */
                 if ((yyvsp[-2].decl).id && (yyvsp[-7].id) && Cmp((yyvsp[-9].id),"typedef") == 0) {
		   String *name = NewString((yyvsp[-2].decl).id);
                   Setattr((yyval.node), "parser:makename", name);
		   Delete(name);
                 }

		 add_symbols((yyval.node));       /* Add enum to tag space */
		 set_nextSibling((yyval.node),n);
		 Delete(n);

		 if (scopedenum) {
		   Swig_symbol_newscope();
		   Swig_symbol_setscopename((yyvsp[-7].id));
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		 }

		 add_symbols((yyvsp[-4].node));      /* Add enum values to appropriate enum or enum class scope */

		 if (scopedenum) {
		   Setattr((yyval.node),"symtab", Swig_symbol_popscope());
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		 }

	         add_symbols(n);
		 Delete(unnamed);
	       }
#line 6719 "y.tab.c" /* yacc.c:1646  */
    break;

  case 154:
#line 3303 "parser.y" /* yacc.c:1646  */
    {
                   /* This is a sick hack.  If the ctor_end has parameters,
                      and the parms parameter only has 1 parameter, this
                      could be a declaration of the form:

                         type (id)(parms)

			 Otherwise it's an error. */
                    int err = 0;
                    (yyval.node) = 0;

		    if ((ParmList_len((yyvsp[-2].pl)) == 1) && (!Swig_scopename_check((yyvsp[-4].type)))) {
		      SwigType *ty = Getattr((yyvsp[-2].pl),"type");
		      String *name = Getattr((yyvsp[-2].pl),"name");
		      err = 1;
		      if (!name) {
			(yyval.node) = new_node("cdecl");
			Setattr((yyval.node),"type",(yyvsp[-4].type));
			Setattr((yyval.node),"storage",(yyvsp[-5].id));
			Setattr((yyval.node),"name",ty);

			if ((yyvsp[0].decl).have_parms) {
			  SwigType *decl = NewStringEmpty();
			  SwigType_add_function(decl,(yyvsp[0].decl).parms);
			  Setattr((yyval.node),"decl",decl);
			  Setattr((yyval.node),"parms",(yyvsp[0].decl).parms);
			  if (Len(scanner_ccode)) {
			    String *code = Copy(scanner_ccode);
			    Setattr((yyval.node),"code",code);
			    Delete(code);
			  }
			}
			if ((yyvsp[0].decl).defarg) {
			  Setattr((yyval.node),"value",(yyvsp[0].decl).defarg);
			}
			Setattr((yyval.node),"throws",(yyvsp[0].decl).throws);
			Setattr((yyval.node),"throw",(yyvsp[0].decl).throwf);
			Setattr((yyval.node),"noexcept",(yyvsp[0].decl).nexcept);
			err = 0;
		      }
		    }
		    if (err) {
		      Swig_error(cparse_file,cparse_line,"Syntax error in input(2).\n");
		      exit(1);
		    }
                }
#line 6770 "y.tab.c" /* yacc.c:1646  */
    break;

  case 155:
#line 3355 "parser.y" /* yacc.c:1646  */
    {  (yyval.node) = (yyvsp[0].node); }
#line 6776 "y.tab.c" /* yacc.c:1646  */
    break;

  case 156:
#line 3356 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 6782 "y.tab.c" /* yacc.c:1646  */
    break;

  case 157:
#line 3357 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 6788 "y.tab.c" /* yacc.c:1646  */
    break;

  case 158:
#line 3358 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 6794 "y.tab.c" /* yacc.c:1646  */
    break;

  case 159:
#line 3359 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 6800 "y.tab.c" /* yacc.c:1646  */
    break;

  case 160:
#line 3360 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = 0; }
#line 6806 "y.tab.c" /* yacc.c:1646  */
    break;

  case 161:
#line 3365 "parser.y" /* yacc.c:1646  */
    {
                   String *prefix;
                   List *bases = 0;
		   Node *scope = 0;
		   String *code;
		   (yyval.node) = new_node("class");
		   Setline((yyval.node),cparse_start_line);
		   Setattr((yyval.node),"kind",(yyvsp[-3].id));
		   if ((yyvsp[-1].bases)) {
		     Setattr((yyval.node),"baselist", Getattr((yyvsp[-1].bases),"public"));
		     Setattr((yyval.node),"protectedbaselist", Getattr((yyvsp[-1].bases),"protected"));
		     Setattr((yyval.node),"privatebaselist", Getattr((yyvsp[-1].bases),"private"));
		   }
		   Setattr((yyval.node),"allows_typedef","1");

		   /* preserve the current scope */
		   Setattr((yyval.node),"prev_symtab",Swig_symbol_current());
		  
		   /* If the class name is qualified.  We need to create or lookup namespace/scope entries */
		   scope = resolve_create_node_scope((yyvsp[-2].str));
		   /* save nscope_inner to the class - it may be overwritten in nested classes*/
		   Setattr((yyval.node), "nested:innerscope", nscope_inner);
		   Setattr((yyval.node), "nested:nscope", nscope);
		   Setfile(scope,cparse_file);
		   Setline(scope,cparse_line);
		   (yyvsp[-2].str) = scope;
		   Setattr((yyval.node),"name",(yyvsp[-2].str));

		   if (currentOuterClass) {
		     SetFlag((yyval.node), "nested");
		     Setattr((yyval.node), "nested:outer", currentOuterClass);
		     set_access_mode((yyval.node));
		   }
		   Swig_features_get(Swig_cparse_features(), Namespaceprefix, Getattr((yyval.node), "name"), 0, (yyval.node));
		   /* save yyrename to the class attribute, to be used later in add_symbols()*/
		   Setattr((yyval.node), "class_rename", make_name((yyval.node), (yyvsp[-2].str), 0));
		   Setattr((yyval.node), "Classprefix", (yyvsp[-2].str));
		   Classprefix = NewString((yyvsp[-2].str));
		   /* Deal with inheritance  */
		   if ((yyvsp[-1].bases))
		     bases = Swig_make_inherit_list((yyvsp[-2].str),Getattr((yyvsp[-1].bases),"public"),Namespaceprefix);
		   prefix = SwigType_istemplate_templateprefix((yyvsp[-2].str));
		   if (prefix) {
		     String *fbase, *tbase;
		     if (Namespaceprefix) {
		       fbase = NewStringf("%s::%s", Namespaceprefix,(yyvsp[-2].str));
		       tbase = NewStringf("%s::%s", Namespaceprefix, prefix);
		     } else {
		       fbase = Copy((yyvsp[-2].str));
		       tbase = Copy(prefix);
		     }
		     Swig_name_inherit(tbase,fbase);
		     Delete(fbase);
		     Delete(tbase);
		   }
                   if (strcmp((yyvsp[-3].id),"class") == 0) {
		     cplus_mode = CPLUS_PRIVATE;
		   } else {
		     cplus_mode = CPLUS_PUBLIC;
		   }
		   if (!cparse_cplusplus) {
		     set_scope_to_global();
		   }
		   Swig_symbol_newscope();
		   Swig_symbol_setscopename((yyvsp[-2].str));
		   Swig_inherit_base_symbols(bases);
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		   cparse_start_line = cparse_line;

		   /* If there are active template parameters, we need to make sure they are
                      placed in the class symbol table so we can catch shadows */

		   if (template_parameters) {
		     Parm *tp = template_parameters;
		     while(tp) {
		       String *tpname = Copy(Getattr(tp,"name"));
		       Node *tn = new_node("templateparm");
		       Setattr(tn,"name",tpname);
		       Swig_symbol_cadd(tpname,tn);
		       tp = nextSibling(tp);
		       Delete(tpname);
		     }
		   }
		   Delete(prefix);
		   inclass = 1;
		   currentOuterClass = (yyval.node);
		   if (cparse_cplusplusout) {
		     /* save the structure declaration to declare it in global scope for C++ to see */
		     code = get_raw_text_balanced('{', '}');
		     Setattr((yyval.node), "code", code);
		     Delete(code);
		   }
               }
#line 6905 "y.tab.c" /* yacc.c:1646  */
    break;

  case 162:
#line 3458 "parser.y" /* yacc.c:1646  */
    {
		   Node *p;
		   SwigType *ty;
		   Symtab *cscope;
		   Node *am = 0;
		   String *scpname = 0;
		   (void) (yyvsp[-3].node);
		   (yyval.node) = currentOuterClass;
		   currentOuterClass = Getattr((yyval.node), "nested:outer");
		   nscope_inner = Getattr((yyval.node), "nested:innerscope");
		   nscope = Getattr((yyval.node), "nested:nscope");
		   Delattr((yyval.node), "nested:innerscope");
		   Delattr((yyval.node), "nested:nscope");
		   if (nscope_inner && Strcmp(nodeType(nscope_inner), "class") == 0) { /* actual parent class for this class */
		     Node* forward_declaration = Swig_symbol_clookup_no_inherit(Getattr((yyval.node),"name"), Getattr(nscope_inner, "symtab"));
		     if (forward_declaration) {
		       Setattr((yyval.node), "access", Getattr(forward_declaration, "access"));
		     }
		     Setattr((yyval.node), "nested:outer", nscope_inner);
		     SetFlag((yyval.node), "nested");
                   }
		   if (!currentOuterClass)
		     inclass = 0;
		   cscope = Getattr((yyval.node), "prev_symtab");
		   Delattr((yyval.node), "prev_symtab");
		   
		   /* Check for pure-abstract class */
		   Setattr((yyval.node),"abstracts", pure_abstracts((yyvsp[-2].node)));
		   
		   /* This bit of code merges in a previously defined %extend directive (if any) */
		   {
		     String *clsname = Swig_symbol_qualifiedscopename(0);
		     am = Getattr(Swig_extend_hash(), clsname);
		     if (am) {
		       Swig_extend_merge((yyval.node), am);
		       Delattr(Swig_extend_hash(), clsname);
		     }
		     Delete(clsname);
		   }
		   if (!classes) classes = NewHash();
		   scpname = Swig_symbol_qualifiedscopename(0);
		   Setattr(classes, scpname, (yyval.node));

		   appendChild((yyval.node), (yyvsp[-2].node));
		   
		   if (am) 
		     Swig_extend_append_previous((yyval.node), am);

		   p = (yyvsp[0].node);
		   if (p && !nscope_inner) {
		     if (!cparse_cplusplus && currentOuterClass)
		       appendChild(currentOuterClass, p);
		     else
		      appendSibling((yyval.node), p);
		   }
		   
		   if (nscope_inner) {
		     ty = NewString(scpname); /* if the class is declared out of scope, let the declarator use fully qualified type*/
		   } else if (cparse_cplusplus && !cparse_externc) {
		     ty = NewString((yyvsp[-6].str));
		   } else {
		     ty = NewStringf("%s %s", (yyvsp[-7].id), (yyvsp[-6].str));
		   }
		   while (p) {
		     Setattr(p, "storage", (yyvsp[-8].id));
		     Setattr(p, "type" ,ty);
		     if (!cparse_cplusplus && currentOuterClass && (!Getattr(currentOuterClass, "name"))) {
		       SetFlag(p, "hasconsttype");
		       SetFlag(p, "feature:immutable");
		     }
		     p = nextSibling(p);
		   }
		   if ((yyvsp[0].node) && Cmp((yyvsp[-8].id),"typedef") == 0)
		     add_typedef_name((yyval.node), (yyvsp[0].node), (yyvsp[-6].str), cscope, scpname);
		   Delete(scpname);

		   if (cplus_mode != CPLUS_PUBLIC) {
		   /* we 'open' the class at the end, to allow %template
		      to add new members */
		     Node *pa = new_node("access");
		     Setattr(pa, "kind", "public");
		     cplus_mode = CPLUS_PUBLIC;
		     appendChild((yyval.node), pa);
		     Delete(pa);
		   }
		   if (currentOuterClass)
		     restore_access_mode((yyval.node));
		   Setattr((yyval.node), "symtab", Swig_symbol_popscope());
		   Classprefix = Getattr((yyval.node), "Classprefix");
		   Delattr((yyval.node), "Classprefix");
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		   if (cplus_mode == CPLUS_PRIVATE) {
		     (yyval.node) = 0; /* skip private nested classes */
		   } else if (cparse_cplusplus && currentOuterClass && ignore_nested_classes && !GetFlag((yyval.node), "feature:flatnested")) {
		     (yyval.node) = nested_forward_declaration((yyvsp[-8].id), (yyvsp[-7].id), (yyvsp[-6].str), Copy((yyvsp[-6].str)), (yyvsp[0].node));
		   } else if (nscope_inner) {
		     /* this is tricky */
		     /* we add the declaration in the original namespace */
		     if (Strcmp(nodeType(nscope_inner), "class") == 0 && cparse_cplusplus && ignore_nested_classes && !GetFlag((yyval.node), "feature:flatnested"))
		       (yyval.node) = nested_forward_declaration((yyvsp[-8].id), (yyvsp[-7].id), (yyvsp[-6].str), Copy((yyvsp[-6].str)), (yyvsp[0].node));
		     appendChild(nscope_inner, (yyval.node));
		     Swig_symbol_setscope(Getattr(nscope_inner, "symtab"));
		     Delete(Namespaceprefix);
		     Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		     yyrename = Copy(Getattr((yyval.node), "class_rename"));
		     add_symbols((yyval.node));
		     Delattr((yyval.node), "class_rename");
		     /* but the variable definition in the current scope */
		     Swig_symbol_setscope(cscope);
		     Delete(Namespaceprefix);
		     Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		     add_symbols((yyvsp[0].node));
		     if (nscope) {
		       (yyval.node) = nscope; /* here we return recreated namespace tower instead of the class itself */
		       if ((yyvsp[0].node)) {
			 appendSibling((yyval.node), (yyvsp[0].node));
		       }
		     } else if (!SwigType_istemplate(ty) && template_parameters == 0) { /* for tempalte we need the class itself */
		       (yyval.node) = (yyvsp[0].node);
		     }
		   } else {
		     Delete(yyrename);
		     yyrename = 0;
		     if (!cparse_cplusplus && currentOuterClass) { /* nested C structs go into global scope*/
		       Node *outer = currentOuterClass;
		       while (Getattr(outer, "nested:outer"))
			 outer = Getattr(outer, "nested:outer");
		       appendSibling(outer, (yyval.node));
		       add_symbols((yyvsp[0].node));
		       set_scope_to_global();
		       Delete(Namespaceprefix);
		       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		       yyrename = Copy(Getattr((yyval.node), "class_rename"));
		       add_symbols((yyval.node));
		       if (!cparse_cplusplusout)
			 Delattr((yyval.node), "nested:outer");
		       Delattr((yyval.node), "class_rename");
		       (yyval.node) = 0;
		     } else {
		       yyrename = Copy(Getattr((yyval.node), "class_rename"));
		       add_symbols((yyval.node));
		       add_symbols((yyvsp[0].node));
		       Delattr((yyval.node), "class_rename");
		     }
		   }
		   Delete(ty);
		   Swig_symbol_setscope(cscope);
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	       }
#line 7061 "y.tab.c" /* yacc.c:1646  */
    break;

  case 163:
#line 3612 "parser.y" /* yacc.c:1646  */
    {
	       String *unnamed;
	       String *code;
	       unnamed = make_unnamed();
	       (yyval.node) = new_node("class");
	       Setline((yyval.node),cparse_start_line);
	       Setattr((yyval.node),"kind",(yyvsp[-2].id));
	       if ((yyvsp[-1].bases)) {
		 Setattr((yyval.node),"baselist", Getattr((yyvsp[-1].bases),"public"));
		 Setattr((yyval.node),"protectedbaselist", Getattr((yyvsp[-1].bases),"protected"));
		 Setattr((yyval.node),"privatebaselist", Getattr((yyvsp[-1].bases),"private"));
	       }
	       Setattr((yyval.node),"storage",(yyvsp[-3].id));
	       Setattr((yyval.node),"unnamed",unnamed);
	       Setattr((yyval.node),"allows_typedef","1");
	       if (currentOuterClass) {
		 SetFlag((yyval.node), "nested");
		 Setattr((yyval.node), "nested:outer", currentOuterClass);
		 set_access_mode((yyval.node));
	       }
	       Swig_features_get(Swig_cparse_features(), Namespaceprefix, 0, 0, (yyval.node));
	       /* save yyrename to the class attribute, to be used later in add_symbols()*/
	       Setattr((yyval.node), "class_rename", make_name((yyval.node),0,0));
	       if (strcmp((yyvsp[-2].id),"class") == 0) {
		 cplus_mode = CPLUS_PRIVATE;
	       } else {
		 cplus_mode = CPLUS_PUBLIC;
	       }
	       Swig_symbol_newscope();
	       cparse_start_line = cparse_line;
	       currentOuterClass = (yyval.node);
	       inclass = 1;
	       Classprefix = NewStringEmpty();
	       Delete(Namespaceprefix);
	       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	       /* save the structure declaration to make a typedef for it later*/
	       code = get_raw_text_balanced('{', '}');
	       Setattr((yyval.node), "code", code);
	       Delete(code);
	     }
#line 7106 "y.tab.c" /* yacc.c:1646  */
    break;

  case 164:
#line 3651 "parser.y" /* yacc.c:1646  */
    {
	       String *unnamed;
               List *bases = 0;
	       String *name = 0;
	       Node *n;
	       Classprefix = 0;
	       (yyval.node) = currentOuterClass;
	       currentOuterClass = Getattr((yyval.node), "nested:outer");
	       if (!currentOuterClass)
		 inclass = 0;
	       else
		 restore_access_mode((yyval.node));
	       unnamed = Getattr((yyval.node),"unnamed");
               /* Check for pure-abstract class */
	       Setattr((yyval.node),"abstracts", pure_abstracts((yyvsp[-2].node)));
	       n = (yyvsp[0].node);
	       if (cparse_cplusplus && currentOuterClass && ignore_nested_classes && !GetFlag((yyval.node), "feature:flatnested")) {
		 String *name = n ? Copy(Getattr(n, "name")) : 0;
		 (yyval.node) = nested_forward_declaration((yyvsp[-7].id), (yyvsp[-6].id), 0, name, n);
		 Swig_symbol_popscope();
	         Delete(Namespaceprefix);
		 Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	       } else if (n) {
	         appendSibling((yyval.node),n);
		 /* If a proper typedef name was given, we'll use it to set the scope name */
		 name = try_to_find_a_name_for_unnamed_structure((yyvsp[-7].id), n);
		 if (name) {
		   String *scpname = 0;
		   SwigType *ty;
		   Setattr((yyval.node),"tdname",name);
		   Setattr((yyval.node),"name",name);
		   Swig_symbol_setscopename(name);
		   if ((yyvsp[-5].bases))
		     bases = Swig_make_inherit_list(name,Getattr((yyvsp[-5].bases),"public"),Namespaceprefix);
		   Swig_inherit_base_symbols(bases);

		     /* If a proper name was given, we use that as the typedef, not unnamed */
		   Clear(unnamed);
		   Append(unnamed, name);
		   if (cparse_cplusplus && !cparse_externc) {
		     ty = NewString(name);
		   } else {
		     ty = NewStringf("%s %s", (yyvsp[-6].id),name);
		   }
		   while (n) {
		     Setattr(n,"storage",(yyvsp[-7].id));
		     Setattr(n, "type", ty);
		     if (!cparse_cplusplus && currentOuterClass && (!Getattr(currentOuterClass, "name"))) {
		       SetFlag(n,"hasconsttype");
		       SetFlag(n,"feature:immutable");
		     }
		     n = nextSibling(n);
		   }
		   n = (yyvsp[0].node);

		   /* Check for previous extensions */
		   {
		     String *clsname = Swig_symbol_qualifiedscopename(0);
		     Node *am = Getattr(Swig_extend_hash(),clsname);
		     if (am) {
		       /* Merge the extension into the symbol table */
		       Swig_extend_merge((yyval.node),am);
		       Swig_extend_append_previous((yyval.node),am);
		       Delattr(Swig_extend_hash(),clsname);
		     }
		     Delete(clsname);
		   }
		   if (!classes) classes = NewHash();
		   scpname = Swig_symbol_qualifiedscopename(0);
		   Setattr(classes,scpname,(yyval.node));
		   Delete(scpname);
		 } else { /* no suitable name was found for a struct */
		   Setattr((yyval.node), "nested:unnamed", Getattr(n, "name")); /* save the name of the first declarator for later use in name generation*/
		   while (n) { /* attach unnamed struct to the declarators, so that they would receive proper type later*/
		     Setattr(n, "nested:unnamedtype", (yyval.node));
		     Setattr(n, "storage", (yyvsp[-7].id));
		     n = nextSibling(n);
		   }
		   n = (yyvsp[0].node);
		   Swig_symbol_setscopename("<unnamed>");
		 }
		 appendChild((yyval.node),(yyvsp[-2].node));
		 /* Pop the scope */
		 Setattr((yyval.node),"symtab",Swig_symbol_popscope());
		 if (name) {
		   Delete(yyrename);
		   yyrename = Copy(Getattr((yyval.node), "class_rename"));
		   Delete(Namespaceprefix);
		   Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		   add_symbols((yyval.node));
		   add_symbols(n);
		   Delattr((yyval.node), "class_rename");
		 }else if (cparse_cplusplus)
		   (yyval.node) = 0; /* ignore unnamed structs for C++ */
	         Delete(unnamed);
	       } else { /* unnamed struct w/o declarator*/
		 Swig_symbol_popscope();
	         Delete(Namespaceprefix);
		 Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		 add_symbols((yyvsp[-2].node));
		 Delete((yyval.node));
		 (yyval.node) = (yyvsp[-2].node); /* pass member list to outer class/namespace (instead of self)*/
	       }
              }
#line 7215 "y.tab.c" /* yacc.c:1646  */
    break;

  case 165:
#line 3757 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = 0; }
#line 7221 "y.tab.c" /* yacc.c:1646  */
    break;

  case 166:
#line 3758 "parser.y" /* yacc.c:1646  */
    {
                        (yyval.node) = new_node("cdecl");
                        Setattr((yyval.node),"name",(yyvsp[-2].decl).id);
                        Setattr((yyval.node),"decl",(yyvsp[-2].decl).type);
                        Setattr((yyval.node),"parms",(yyvsp[-2].decl).parms);
			set_nextSibling((yyval.node),(yyvsp[0].node));
                    }
#line 7233 "y.tab.c" /* yacc.c:1646  */
    break;

  case 167:
#line 3770 "parser.y" /* yacc.c:1646  */
    {
              if ((yyvsp[-3].id) && (Strcmp((yyvsp[-3].id),"friend") == 0)) {
		/* Ignore */
                (yyval.node) = 0; 
	      } else {
		(yyval.node) = new_node("classforward");
		Setattr((yyval.node),"kind",(yyvsp[-2].id));
		Setattr((yyval.node),"name",(yyvsp[-1].str));
		Setattr((yyval.node),"sym:weak", "1");
		add_symbols((yyval.node));
	      }
             }
#line 7250 "y.tab.c" /* yacc.c:1646  */
    break;

  case 168:
#line 3788 "parser.y" /* yacc.c:1646  */
    { 
		   if (currentOuterClass)
		     Setattr(currentOuterClass, "template_parameters", template_parameters);
		    template_parameters = (yyvsp[-1].tparms); 
		  }
#line 7260 "y.tab.c" /* yacc.c:1646  */
    break;

  case 169:
#line 3792 "parser.y" /* yacc.c:1646  */
    {
			String *tname = 0;
			int     error = 0;

			/* check if we get a namespace node with a class declaration, and retrieve the class */
			Symtab *cscope = Swig_symbol_current();
			Symtab *sti = 0;
			Node *ntop = (yyvsp[0].node);
			Node *ni = ntop;
			SwigType *ntype = ni ? nodeType(ni) : 0;
			while (ni && Strcmp(ntype,"namespace") == 0) {
			  sti = Getattr(ni,"symtab");
			  ni = firstChild(ni);
			  ntype = nodeType(ni);
			}
			if (sti) {
			  Swig_symbol_setscope(sti);
			  Delete(Namespaceprefix);
			  Namespaceprefix = Swig_symbol_qualifiedscopename(0);
			  (yyvsp[0].node) = ni;
			}

			(yyval.node) = (yyvsp[0].node);
			if ((yyval.node)) tname = Getattr((yyval.node),"name");
			
			/* Check if the class is a template specialization */
			if (((yyval.node)) && (Strchr(tname,'<')) && (!is_operator(tname))) {
			  /* If a specialization.  Check if defined. */
			  Node *tempn = 0;
			  {
			    String *tbase = SwigType_templateprefix(tname);
			    tempn = Swig_symbol_clookup_local(tbase,0);
			    if (!tempn || (Strcmp(nodeType(tempn),"template") != 0)) {
			      SWIG_WARN_NODE_BEGIN(tempn);
			      Swig_warning(WARN_PARSE_TEMPLATE_SP_UNDEF, Getfile((yyval.node)),Getline((yyval.node)),"Specialization of non-template '%s'.\n", tbase);
			      SWIG_WARN_NODE_END(tempn);
			      tempn = 0;
			      error = 1;
			    }
			    Delete(tbase);
			  }
			  Setattr((yyval.node),"specialization","1");
			  Setattr((yyval.node),"templatetype",nodeType((yyval.node)));
			  set_nodeType((yyval.node),"template");
			  /* Template partial specialization */
			  if (tempn && ((yyvsp[-3].tparms)) && ((yyvsp[0].node))) {
			    List   *tlist;
			    String *targs = SwigType_templateargs(tname);
			    tlist = SwigType_parmlist(targs);
			    /*			  Printf(stdout,"targs = '%s' %s\n", targs, tlist); */
			    if (!Getattr((yyval.node),"sym:weak")) {
			      Setattr((yyval.node),"sym:typename","1");
			    }
			    
			    if (Len(tlist) != ParmList_len(Getattr(tempn,"templateparms"))) {
			      Swig_error(Getfile((yyval.node)),Getline((yyval.node)),"Inconsistent argument count in template partial specialization. %d %d\n", Len(tlist), ParmList_len(Getattr(tempn,"templateparms")));
			      
			    } else {

			    /* This code builds the argument list for the partial template
			       specialization.  This is a little hairy, but the idea is as
			       follows:

			       $3 contains a list of arguments supplied for the template.
			       For example template<class T>.

			       tlist is a list of the specialization arguments--which may be
			       different.  For example class<int,T>.

			       tp is a copy of the arguments in the original template definition.
       
			       The patching algorithm walks through the list of supplied
			       arguments ($3), finds the position in the specialization arguments
			       (tlist), and then patches the name in the argument list of the
			       original template.
			    */

			    {
			      String *pn;
			      Parm *p, *p1;
			      int i, nargs;
			      Parm *tp = CopyParmList(Getattr(tempn,"templateparms"));
			      nargs = Len(tlist);
			      p = (yyvsp[-3].tparms);
			      while (p) {
				for (i = 0; i < nargs; i++){
				  pn = Getattr(p,"name");
				  if (Strcmp(pn,SwigType_base(Getitem(tlist,i))) == 0) {
				    int j;
				    Parm *p1 = tp;
				    for (j = 0; j < i; j++) {
				      p1 = nextSibling(p1);
				    }
				    Setattr(p1,"name",pn);
				    Setattr(p1,"partialarg","1");
				  }
				}
				p = nextSibling(p);
			      }
			      p1 = tp;
			      i = 0;
			      while (p1) {
				if (!Getattr(p1,"partialarg")) {
				  Delattr(p1,"name");
				  Setattr(p1,"type", Getitem(tlist,i));
				} 
				i++;
				p1 = nextSibling(p1);
			      }
			      Setattr((yyval.node),"templateparms",tp);
			      Delete(tp);
			    }
  #if 0
			    /* Patch the parameter list */
			    if (tempn) {
			      Parm *p,*p1;
			      ParmList *tp = CopyParmList(Getattr(tempn,"templateparms"));
			      p = (yyvsp[-3].tparms);
			      p1 = tp;
			      while (p && p1) {
				String *pn = Getattr(p,"name");
				Printf(stdout,"pn = '%s'\n", pn);
				if (pn) Setattr(p1,"name",pn);
				else Delattr(p1,"name");
				pn = Getattr(p,"type");
				if (pn) Setattr(p1,"type",pn);
				p = nextSibling(p);
				p1 = nextSibling(p1);
			      }
			      Setattr((yyval.node),"templateparms",tp);
			      Delete(tp);
			    } else {
			      Setattr((yyval.node),"templateparms",(yyvsp[-3].tparms));
			    }
  #endif
			    Delattr((yyval.node),"specialization");
			    Setattr((yyval.node),"partialspecialization","1");
			    /* Create a specialized name for matching */
			    {
			      Parm *p = (yyvsp[-3].tparms);
			      String *fname = NewString(Getattr((yyval.node),"name"));
			      String *ffname = 0;
			      ParmList *partialparms = 0;

			      char   tmp[32];
			      int    i, ilen;
			      while (p) {
				String *n = Getattr(p,"name");
				if (!n) {
				  p = nextSibling(p);
				  continue;
				}
				ilen = Len(tlist);
				for (i = 0; i < ilen; i++) {
				  if (Strstr(Getitem(tlist,i),n)) {
				    sprintf(tmp,"$%d",i+1);
				    Replaceid(fname,n,tmp);
				  }
				}
				p = nextSibling(p);
			      }
			      /* Patch argument names with typedef */
			      {
				Iterator tt;
				Parm *parm_current = 0;
				List *tparms = SwigType_parmlist(fname);
				ffname = SwigType_templateprefix(fname);
				Append(ffname,"<(");
				for (tt = First(tparms); tt.item; ) {
				  SwigType *rtt = Swig_symbol_typedef_reduce(tt.item,0);
				  SwigType *ttr = Swig_symbol_type_qualify(rtt,0);

				  Parm *newp = NewParmWithoutFileLineInfo(ttr, 0);
				  if (partialparms)
				    set_nextSibling(parm_current, newp);
				  else
				    partialparms = newp;
				  parm_current = newp;

				  Append(ffname,ttr);
				  tt = Next(tt);
				  if (tt.item) Putc(',',ffname);
				  Delete(rtt);
				  Delete(ttr);
				}
				Delete(tparms);
				Append(ffname,")>");
			      }
			      {
				Node *new_partial = NewHash();
				String *partials = Getattr(tempn,"partials");
				if (!partials) {
				  partials = NewList();
				  Setattr(tempn,"partials",partials);
				  Delete(partials);
				}
				/*			      Printf(stdout,"partial: fname = '%s', '%s'\n", fname, Swig_symbol_typedef_reduce(fname,0)); */
				Setattr(new_partial, "partialparms", partialparms);
				Setattr(new_partial, "templcsymname", ffname);
				Append(partials, new_partial);
			      }
			      Setattr((yyval.node),"partialargs",ffname);
			      Swig_symbol_cadd(ffname,(yyval.node));
			    }
			    }
			    Delete(tlist);
			    Delete(targs);
			  } else {
			    /* An explicit template specialization */
			    /* add default args from primary (unspecialized) template */
			    String *ty = Swig_symbol_template_deftype(tname,0);
			    String *fname = Swig_symbol_type_qualify(ty,0);
			    Swig_symbol_cadd(fname,(yyval.node));
			    Delete(ty);
			    Delete(fname);
			  }
			}  else if ((yyval.node)) {
			  Setattr((yyval.node),"templatetype",nodeType((yyvsp[0].node)));
			  set_nodeType((yyval.node),"template");
			  Setattr((yyval.node),"templateparms", (yyvsp[-3].tparms));
			  if (!Getattr((yyval.node),"sym:weak")) {
			    Setattr((yyval.node),"sym:typename","1");
			  }
			  add_symbols((yyval.node));
			  default_arguments((yyval.node));
			  /* We also place a fully parameterized version in the symbol table */
			  {
			    Parm *p;
			    String *fname = NewStringf("%s<(", Getattr((yyval.node),"name"));
			    p = (yyvsp[-3].tparms);
			    while (p) {
			      String *n = Getattr(p,"name");
			      if (!n) n = Getattr(p,"type");
			      Append(fname,n);
			      p = nextSibling(p);
			      if (p) Putc(',',fname);
			    }
			    Append(fname,")>");
			    Swig_symbol_cadd(fname,(yyval.node));
			  }
			}
			(yyval.node) = ntop;
			Swig_symbol_setscope(cscope);
			Delete(Namespaceprefix);
			Namespaceprefix = Swig_symbol_qualifiedscopename(0);
			if (error || (nscope_inner && Strcmp(nodeType(nscope_inner), "class") == 0)) {
			  (yyval.node) = 0;
			}
			if (currentOuterClass)
			  template_parameters = Getattr(currentOuterClass, "template_parameters");
			else
			  template_parameters = 0;
                }
#line 7518 "y.tab.c" /* yacc.c:1646  */
    break;

  case 170:
#line 4047 "parser.y" /* yacc.c:1646  */
    {
		  Swig_warning(WARN_PARSE_EXPLICIT_TEMPLATE, cparse_file, cparse_line, "Explicit template instantiation ignored.\n");
                  (yyval.node) = 0; 
		}
#line 7527 "y.tab.c" /* yacc.c:1646  */
    break;

  case 171:
#line 4053 "parser.y" /* yacc.c:1646  */
    {
		  Swig_warning(WARN_PARSE_EXPLICIT_TEMPLATE, cparse_file, cparse_line, "Explicit template instantiation ignored.\n");
                  (yyval.node) = 0; 
                }
#line 7536 "y.tab.c" /* yacc.c:1646  */
    break;

  case 172:
#line 4059 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.node) = (yyvsp[0].node);
                }
#line 7544 "y.tab.c" /* yacc.c:1646  */
    break;

  case 173:
#line 4062 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.node) = (yyvsp[0].node);
                }
#line 7552 "y.tab.c" /* yacc.c:1646  */
    break;

  case 174:
#line 4065 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.node) = (yyvsp[0].node);
                }
#line 7560 "y.tab.c" /* yacc.c:1646  */
    break;

  case 175:
#line 4068 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.node) = (yyvsp[0].node);
                }
#line 7568 "y.tab.c" /* yacc.c:1646  */
    break;

  case 176:
#line 4071 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.node) = 0;
                }
#line 7576 "y.tab.c" /* yacc.c:1646  */
    break;

  case 177:
#line 4074 "parser.y" /* yacc.c:1646  */
    {
                  (yyval.node) = (yyvsp[0].node);
                }
#line 7584 "y.tab.c" /* yacc.c:1646  */
    break;

  case 178:
#line 4077 "parser.y" /* yacc.c:1646  */
    {
                  (yyval.node) = (yyvsp[0].node);
                }
#line 7592 "y.tab.c" /* yacc.c:1646  */
    break;

  case 179:
#line 4082 "parser.y" /* yacc.c:1646  */
    {
		   /* Rip out the parameter names */
		  Parm *p = (yyvsp[0].pl);
		  (yyval.tparms) = (yyvsp[0].pl);

		  while (p) {
		    String *name = Getattr(p,"name");
		    if (!name) {
		      /* Hmmm. Maybe it's a 'class T' parameter */
		      char *type = Char(Getattr(p,"type"));
		      /* Template template parameter */
		      if (strncmp(type,"template<class> ",16) == 0) {
			type += 16;
		      }
		      if ((strncmp(type,"class ",6) == 0) || (strncmp(type,"typename ", 9) == 0)) {
			char *t = strchr(type,' ');
			Setattr(p,"name", t+1);
		      } else 
                      /* Variadic template args */
		      if ((strncmp(type,"class... ",9) == 0) || (strncmp(type,"typename... ", 12) == 0)) {
			char *t = strchr(type,' ');
			Setattr(p,"name", t+1);
			Setattr(p,"variadic", "1");
		      } else {
			/*
			 Swig_error(cparse_file, cparse_line, "Missing template parameter name\n");
			 $$.rparms = 0;
			 $$.parms = 0;
			 break; */
		      }
		    }
		    p = nextSibling(p);
		  }
                 }
#line 7631 "y.tab.c" /* yacc.c:1646  */
    break;

  case 180:
#line 4118 "parser.y" /* yacc.c:1646  */
    {
                      set_nextSibling((yyvsp[-1].p),(yyvsp[0].pl));
                      (yyval.pl) = (yyvsp[-1].p);
                   }
#line 7640 "y.tab.c" /* yacc.c:1646  */
    break;

  case 181:
#line 4122 "parser.y" /* yacc.c:1646  */
    { (yyval.pl) = 0; }
#line 7646 "y.tab.c" /* yacc.c:1646  */
    break;

  case 182:
#line 4125 "parser.y" /* yacc.c:1646  */
    {
		    (yyval.p) = NewParmWithoutFileLineInfo(NewString((yyvsp[0].id)), 0);
                  }
#line 7654 "y.tab.c" /* yacc.c:1646  */
    break;

  case 183:
#line 4128 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.p) = (yyvsp[0].p);
                  }
#line 7662 "y.tab.c" /* yacc.c:1646  */
    break;

  case 184:
#line 4133 "parser.y" /* yacc.c:1646  */
    {
                         set_nextSibling((yyvsp[-1].p),(yyvsp[0].pl));
                         (yyval.pl) = (yyvsp[-1].p);
                       }
#line 7671 "y.tab.c" /* yacc.c:1646  */
    break;

  case 185:
#line 4137 "parser.y" /* yacc.c:1646  */
    { (yyval.pl) = 0; }
#line 7677 "y.tab.c" /* yacc.c:1646  */
    break;

  case 186:
#line 4142 "parser.y" /* yacc.c:1646  */
    {
                  String *uname = Swig_symbol_type_qualify((yyvsp[-1].str),0);
		  String *name = Swig_scopename_last((yyvsp[-1].str));
                  (yyval.node) = new_node("using");
		  Setattr((yyval.node),"uname",uname);
		  Setattr((yyval.node),"name", name);
		  Delete(uname);
		  Delete(name);
		  add_symbols((yyval.node));
             }
#line 7692 "y.tab.c" /* yacc.c:1646  */
    break;

  case 187:
#line 4152 "parser.y" /* yacc.c:1646  */
    {
	       Node *n = Swig_symbol_clookup((yyvsp[-1].str),0);
	       if (!n) {
		 Swig_error(cparse_file, cparse_line, "Nothing known about namespace '%s'\n", (yyvsp[-1].str));
		 (yyval.node) = 0;
	       } else {

		 while (Strcmp(nodeType(n),"using") == 0) {
		   n = Getattr(n,"node");
		 }
		 if (n) {
		   if (Strcmp(nodeType(n),"namespace") == 0) {
		     Symtab *current = Swig_symbol_current();
		     Symtab *symtab = Getattr(n,"symtab");
		     (yyval.node) = new_node("using");
		     Setattr((yyval.node),"node",n);
		     Setattr((yyval.node),"namespace", (yyvsp[-1].str));
		     if (current != symtab) {
		       Swig_symbol_inherit(symtab);
		     }
		   } else {
		     Swig_error(cparse_file, cparse_line, "'%s' is not a namespace.\n", (yyvsp[-1].str));
		     (yyval.node) = 0;
		   }
		 } else {
		   (yyval.node) = 0;
		 }
	       }
             }
#line 7726 "y.tab.c" /* yacc.c:1646  */
    break;

  case 188:
#line 4183 "parser.y" /* yacc.c:1646  */
    { 
                Hash *h;
                (yyvsp[-2].node) = Swig_symbol_current();
		h = Swig_symbol_clookup((yyvsp[-1].str),0);
		if (h && ((yyvsp[-2].node) == Getattr(h,"sym:symtab")) && (Strcmp(nodeType(h),"namespace") == 0)) {
		  if (Getattr(h,"alias")) {
		    h = Getattr(h,"namespace");
		    Swig_warning(WARN_PARSE_NAMESPACE_ALIAS, cparse_file, cparse_line, "Namespace alias '%s' not allowed here. Assuming '%s'\n",
				 (yyvsp[-1].str), Getattr(h,"name"));
		    (yyvsp[-1].str) = Getattr(h,"name");
		  }
		  Swig_symbol_setscope(Getattr(h,"symtab"));
		} else {
		  Swig_symbol_newscope();
		  Swig_symbol_setscopename((yyvsp[-1].str));
		}
		Delete(Namespaceprefix);
		Namespaceprefix = Swig_symbol_qualifiedscopename(0);
             }
#line 7750 "y.tab.c" /* yacc.c:1646  */
    break;

  case 189:
#line 4201 "parser.y" /* yacc.c:1646  */
    {
                Node *n = (yyvsp[-1].node);
		set_nodeType(n,"namespace");
		Setattr(n,"name",(yyvsp[-4].str));
                Setattr(n,"symtab", Swig_symbol_popscope());
		Swig_symbol_setscope((yyvsp[-5].node));
		(yyval.node) = n;
		Delete(Namespaceprefix);
		Namespaceprefix = Swig_symbol_qualifiedscopename(0);
		add_symbols((yyval.node));
             }
#line 7766 "y.tab.c" /* yacc.c:1646  */
    break;

  case 190:
#line 4212 "parser.y" /* yacc.c:1646  */
    {
	       Hash *h;
	       (yyvsp[-1].node) = Swig_symbol_current();
	       h = Swig_symbol_clookup("    ",0);
	       if (h && (Strcmp(nodeType(h),"namespace") == 0)) {
		 Swig_symbol_setscope(Getattr(h,"symtab"));
	       } else {
		 Swig_symbol_newscope();
		 /* we don't use "__unnamed__", but a long 'empty' name */
		 Swig_symbol_setscopename("    ");
	       }
	       Namespaceprefix = 0;
             }
#line 7784 "y.tab.c" /* yacc.c:1646  */
    break;

  case 191:
#line 4224 "parser.y" /* yacc.c:1646  */
    {
	       (yyval.node) = (yyvsp[-1].node);
	       set_nodeType((yyval.node),"namespace");
	       Setattr((yyval.node),"unnamed","1");
	       Setattr((yyval.node),"symtab", Swig_symbol_popscope());
	       Swig_symbol_setscope((yyvsp[-4].node));
	       Delete(Namespaceprefix);
	       Namespaceprefix = Swig_symbol_qualifiedscopename(0);
	       add_symbols((yyval.node));
             }
#line 7799 "y.tab.c" /* yacc.c:1646  */
    break;

  case 192:
#line 4234 "parser.y" /* yacc.c:1646  */
    {
	       /* Namespace alias */
	       Node *n;
	       (yyval.node) = new_node("namespace");
	       Setattr((yyval.node),"name",(yyvsp[-3].id));
	       Setattr((yyval.node),"alias",(yyvsp[-1].str));
	       n = Swig_symbol_clookup((yyvsp[-1].str),0);
	       if (!n) {
		 Swig_error(cparse_file, cparse_line, "Unknown namespace '%s'\n", (yyvsp[-1].str));
		 (yyval.node) = 0;
	       } else {
		 if (Strcmp(nodeType(n),"namespace") != 0) {
		   Swig_error(cparse_file, cparse_line, "'%s' is not a namespace\n",(yyvsp[-1].str));
		   (yyval.node) = 0;
		 } else {
		   while (Getattr(n,"alias")) {
		     n = Getattr(n,"namespace");
		   }
		   Setattr((yyval.node),"namespace",n);
		   add_symbols((yyval.node));
		   /* Set up a scope alias */
		   Swig_symbol_alias((yyvsp[-3].id),Getattr(n,"symtab"));
		 }
	       }
             }
#line 7829 "y.tab.c" /* yacc.c:1646  */
    break;

  case 193:
#line 4261 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.node) = (yyvsp[-1].node);
                   /* Insert cpp_member (including any siblings) to the front of the cpp_members linked list */
		   if ((yyval.node)) {
		     Node *p = (yyval.node);
		     Node *pp =0;
		     while (p) {
		       pp = p;
		       p = nextSibling(p);
		     }
		     set_nextSibling(pp,(yyvsp[0].node));
		     if ((yyvsp[0].node))
		       set_previousSibling((yyvsp[0].node), pp);
		   } else {
		     (yyval.node) = (yyvsp[0].node);
		   }
             }
#line 7851 "y.tab.c" /* yacc.c:1646  */
    break;

  case 194:
#line 4278 "parser.y" /* yacc.c:1646  */
    { 
	       extendmode = 1;
	       if (cplus_mode != CPLUS_PUBLIC) {
		 Swig_error(cparse_file,cparse_line,"%%extend can only be used in a public section\n");
	       }
             }
#line 7862 "y.tab.c" /* yacc.c:1646  */
    break;

  case 195:
#line 4283 "parser.y" /* yacc.c:1646  */
    {
	       extendmode = 0;
	     }
#line 7870 "y.tab.c" /* yacc.c:1646  */
    break;

  case 196:
#line 4285 "parser.y" /* yacc.c:1646  */
    {
	       (yyval.node) = new_node("extend");
	       mark_nodes_as_extend((yyvsp[-3].node));
	       appendChild((yyval.node),(yyvsp[-3].node));
	       set_nextSibling((yyval.node),(yyvsp[0].node));
	     }
#line 7881 "y.tab.c" /* yacc.c:1646  */
    break;

  case 197:
#line 4291 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 7887 "y.tab.c" /* yacc.c:1646  */
    break;

  case 198:
#line 4292 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = 0;}
#line 7893 "y.tab.c" /* yacc.c:1646  */
    break;

  case 199:
#line 4293 "parser.y" /* yacc.c:1646  */
    {
	       int start_line = cparse_line;
	       skip_decl();
	       Swig_error(cparse_file,start_line,"Syntax error in input(3).\n");
	       exit(1);
	       }
#line 7904 "y.tab.c" /* yacc.c:1646  */
    break;

  case 200:
#line 4298 "parser.y" /* yacc.c:1646  */
    { 
		 (yyval.node) = (yyvsp[0].node);
   	     }
#line 7912 "y.tab.c" /* yacc.c:1646  */
    break;

  case 201:
#line 4309 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 7918 "y.tab.c" /* yacc.c:1646  */
    break;

  case 202:
#line 4310 "parser.y" /* yacc.c:1646  */
    { 
                 (yyval.node) = (yyvsp[0].node); 
		 if (extendmode && current_class) {
		   String *symname;
		   symname= make_name((yyval.node),Getattr((yyval.node),"name"), Getattr((yyval.node),"decl"));
		   if (Strcmp(symname,Getattr((yyval.node),"name")) == 0) {
		     /* No renaming operation.  Set name to class name */
		     Delete(yyrename);
		     yyrename = NewString(Getattr(current_class,"sym:name"));
		   } else {
		     Delete(yyrename);
		     yyrename = symname;
		   }
		 }
		 add_symbols((yyval.node));
                 default_arguments((yyval.node));
             }
#line 7940 "y.tab.c" /* yacc.c:1646  */
    break;

  case 203:
#line 4327 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 7946 "y.tab.c" /* yacc.c:1646  */
    break;

  case 204:
#line 4328 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 7952 "y.tab.c" /* yacc.c:1646  */
    break;

  case 205:
#line 4329 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 7958 "y.tab.c" /* yacc.c:1646  */
    break;

  case 206:
#line 4330 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 7964 "y.tab.c" /* yacc.c:1646  */
    break;

  case 207:
#line 4331 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 7970 "y.tab.c" /* yacc.c:1646  */
    break;

  case 208:
#line 4332 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 7976 "y.tab.c" /* yacc.c:1646  */
    break;

  case 209:
#line 4333 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 7982 "y.tab.c" /* yacc.c:1646  */
    break;

  case 210:
#line 4334 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = 0; }
#line 7988 "y.tab.c" /* yacc.c:1646  */
    break;

  case 211:
#line 4335 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 7994 "y.tab.c" /* yacc.c:1646  */
    break;

  case 212:
#line 4336 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 8000 "y.tab.c" /* yacc.c:1646  */
    break;

  case 213:
#line 4337 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = 0; }
#line 8006 "y.tab.c" /* yacc.c:1646  */
    break;

  case 214:
#line 4338 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 8012 "y.tab.c" /* yacc.c:1646  */
    break;

  case 215:
#line 4339 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 8018 "y.tab.c" /* yacc.c:1646  */
    break;

  case 216:
#line 4340 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = 0; }
#line 8024 "y.tab.c" /* yacc.c:1646  */
    break;

  case 217:
#line 4341 "parser.y" /* yacc.c:1646  */
    {(yyval.node) = (yyvsp[0].node); }
#line 8030 "y.tab.c" /* yacc.c:1646  */
    break;

  case 218:
#line 4342 "parser.y" /* yacc.c:1646  */
    {(yyval.node) = (yyvsp[0].node); }
#line 8036 "y.tab.c" /* yacc.c:1646  */
    break;

  case 219:
#line 4343 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = 0; }
#line 8042 "y.tab.c" /* yacc.c:1646  */
    break;

  case 220:
#line 4352 "parser.y" /* yacc.c:1646  */
    {
              if (inclass || extendmode) {
		SwigType *decl = NewStringEmpty();
		(yyval.node) = new_node("constructor");
		Setattr((yyval.node),"storage",(yyvsp[-5].id));
		Setattr((yyval.node),"name",(yyvsp[-4].type));
		Setattr((yyval.node),"parms",(yyvsp[-2].pl));
		SwigType_add_function(decl,(yyvsp[-2].pl));
		Setattr((yyval.node),"decl",decl);
		Setattr((yyval.node),"throws",(yyvsp[0].decl).throws);
		Setattr((yyval.node),"throw",(yyvsp[0].decl).throwf);
		Setattr((yyval.node),"noexcept",(yyvsp[0].decl).nexcept);
		if (Len(scanner_ccode)) {
		  String *code = Copy(scanner_ccode);
		  Setattr((yyval.node),"code",code);
		  Delete(code);
		}
		SetFlag((yyval.node),"feature:new");
		if ((yyvsp[0].decl).defarg)
		  Setattr((yyval.node),"value",(yyvsp[0].decl).defarg);
	      } else {
		(yyval.node) = 0;
              }
              }
#line 8071 "y.tab.c" /* yacc.c:1646  */
    break;

  case 221:
#line 4380 "parser.y" /* yacc.c:1646  */
    {
               String *name = NewStringf("%s",(yyvsp[-4].str));
	       if (*(Char(name)) != '~') Insert(name,0,"~");
               (yyval.node) = new_node("destructor");
	       Setattr((yyval.node),"name",name);
	       Delete(name);
	       if (Len(scanner_ccode)) {
		 String *code = Copy(scanner_ccode);
		 Setattr((yyval.node),"code",code);
		 Delete(code);
	       }
	       {
		 String *decl = NewStringEmpty();
		 SwigType_add_function(decl,(yyvsp[-2].pl));
		 Setattr((yyval.node),"decl",decl);
		 Delete(decl);
	       }
	       Setattr((yyval.node),"throws",(yyvsp[0].dtype).throws);
	       Setattr((yyval.node),"throw",(yyvsp[0].dtype).throwf);
	       Setattr((yyval.node),"noexcept",(yyvsp[0].dtype).nexcept);
	       if ((yyvsp[0].dtype).val)
	         Setattr((yyval.node),"value",(yyvsp[0].dtype).val);
	       add_symbols((yyval.node));
	      }
#line 8100 "y.tab.c" /* yacc.c:1646  */
    break;

  case 222:
#line 4407 "parser.y" /* yacc.c:1646  */
    {
		String *name;
		(yyval.node) = new_node("destructor");
		Setattr((yyval.node),"storage","virtual");
	        name = NewStringf("%s",(yyvsp[-4].str));
		if (*(Char(name)) != '~') Insert(name,0,"~");
		Setattr((yyval.node),"name",name);
		Delete(name);
		Setattr((yyval.node),"throws",(yyvsp[0].dtype).throws);
		Setattr((yyval.node),"throw",(yyvsp[0].dtype).throwf);
		Setattr((yyval.node),"noexcept",(yyvsp[0].dtype).nexcept);
		if ((yyvsp[0].dtype).val)
		  Setattr((yyval.node),"value",(yyvsp[0].dtype).val);
		if (Len(scanner_ccode)) {
		  String *code = Copy(scanner_ccode);
		  Setattr((yyval.node),"code",code);
		  Delete(code);
		}
		{
		  String *decl = NewStringEmpty();
		  SwigType_add_function(decl,(yyvsp[-2].pl));
		  Setattr((yyval.node),"decl",decl);
		  Delete(decl);
		}

		add_symbols((yyval.node));
	      }
#line 8132 "y.tab.c" /* yacc.c:1646  */
    break;

  case 223:
#line 4438 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.node) = new_node("cdecl");
                 Setattr((yyval.node),"type",(yyvsp[-5].type));
		 Setattr((yyval.node),"name",(yyvsp[-6].str));
		 Setattr((yyval.node),"storage",(yyvsp[-7].id));

		 SwigType_add_function((yyvsp[-4].type),(yyvsp[-2].pl));
		 if ((yyvsp[0].dtype).qualifier) {
		   SwigType_push((yyvsp[-4].type),(yyvsp[0].dtype).qualifier);
		 }
		 Setattr((yyval.node),"decl",(yyvsp[-4].type));
		 Setattr((yyval.node),"parms",(yyvsp[-2].pl));
		 Setattr((yyval.node),"conversion_operator","1");
		 add_symbols((yyval.node));
              }
#line 8152 "y.tab.c" /* yacc.c:1646  */
    break;

  case 224:
#line 4453 "parser.y" /* yacc.c:1646  */
    {
		 SwigType *decl;
                 (yyval.node) = new_node("cdecl");
                 Setattr((yyval.node),"type",(yyvsp[-5].type));
		 Setattr((yyval.node),"name",(yyvsp[-6].str));
		 Setattr((yyval.node),"storage",(yyvsp[-7].id));
		 decl = NewStringEmpty();
		 SwigType_add_reference(decl);
		 SwigType_add_function(decl,(yyvsp[-2].pl));
		 if ((yyvsp[0].dtype).qualifier) {
		   SwigType_push(decl,(yyvsp[0].dtype).qualifier);
		 }
		 Setattr((yyval.node),"decl",decl);
		 Setattr((yyval.node),"parms",(yyvsp[-2].pl));
		 Setattr((yyval.node),"conversion_operator","1");
		 add_symbols((yyval.node));
	       }
#line 8174 "y.tab.c" /* yacc.c:1646  */
    break;

  case 225:
#line 4470 "parser.y" /* yacc.c:1646  */
    {
		 SwigType *decl;
                 (yyval.node) = new_node("cdecl");
                 Setattr((yyval.node),"type",(yyvsp[-5].type));
		 Setattr((yyval.node),"name",(yyvsp[-6].str));
		 Setattr((yyval.node),"storage",(yyvsp[-7].id));
		 decl = NewStringEmpty();
		 SwigType_add_rvalue_reference(decl);
		 SwigType_add_function(decl,(yyvsp[-2].pl));
		 if ((yyvsp[0].dtype).qualifier) {
		   SwigType_push(decl,(yyvsp[0].dtype).qualifier);
		 }
		 Setattr((yyval.node),"decl",decl);
		 Setattr((yyval.node),"parms",(yyvsp[-2].pl));
		 Setattr((yyval.node),"conversion_operator","1");
		 add_symbols((yyval.node));
	       }
#line 8196 "y.tab.c" /* yacc.c:1646  */
    break;

  case 226:
#line 4488 "parser.y" /* yacc.c:1646  */
    {
		 SwigType *decl;
                 (yyval.node) = new_node("cdecl");
                 Setattr((yyval.node),"type",(yyvsp[-6].type));
		 Setattr((yyval.node),"name",(yyvsp[-7].str));
		 Setattr((yyval.node),"storage",(yyvsp[-8].id));
		 decl = NewStringEmpty();
		 SwigType_add_pointer(decl);
		 SwigType_add_reference(decl);
		 SwigType_add_function(decl,(yyvsp[-2].pl));
		 if ((yyvsp[0].dtype).qualifier) {
		   SwigType_push(decl,(yyvsp[0].dtype).qualifier);
		 }
		 Setattr((yyval.node),"decl",decl);
		 Setattr((yyval.node),"parms",(yyvsp[-2].pl));
		 Setattr((yyval.node),"conversion_operator","1");
		 add_symbols((yyval.node));
	       }
#line 8219 "y.tab.c" /* yacc.c:1646  */
    break;

  case 227:
#line 4507 "parser.y" /* yacc.c:1646  */
    {
		String *t = NewStringEmpty();
		(yyval.node) = new_node("cdecl");
		Setattr((yyval.node),"type",(yyvsp[-4].type));
		Setattr((yyval.node),"name",(yyvsp[-5].str));
		 Setattr((yyval.node),"storage",(yyvsp[-6].id));
		SwigType_add_function(t,(yyvsp[-2].pl));
		if ((yyvsp[0].dtype).qualifier) {
		  SwigType_push(t,(yyvsp[0].dtype).qualifier);
		}
		Setattr((yyval.node),"decl",t);
		Setattr((yyval.node),"parms",(yyvsp[-2].pl));
		Setattr((yyval.node),"conversion_operator","1");
		add_symbols((yyval.node));
              }
#line 8239 "y.tab.c" /* yacc.c:1646  */
    break;

  case 228:
#line 4526 "parser.y" /* yacc.c:1646  */
    {
                 skip_balanced('{','}');
                 (yyval.node) = 0;
               }
#line 8248 "y.tab.c" /* yacc.c:1646  */
    break;

  case 229:
#line 4533 "parser.y" /* yacc.c:1646  */
    {
                skip_balanced('(',')');
                (yyval.node) = 0;
              }
#line 8257 "y.tab.c" /* yacc.c:1646  */
    break;

  case 230:
#line 4540 "parser.y" /* yacc.c:1646  */
    { 
                (yyval.node) = new_node("access");
		Setattr((yyval.node),"kind","public");
                cplus_mode = CPLUS_PUBLIC;
              }
#line 8267 "y.tab.c" /* yacc.c:1646  */
    break;

  case 231:
#line 4547 "parser.y" /* yacc.c:1646  */
    { 
                (yyval.node) = new_node("access");
                Setattr((yyval.node),"kind","private");
		cplus_mode = CPLUS_PRIVATE;
	      }
#line 8277 "y.tab.c" /* yacc.c:1646  */
    break;

  case 232:
#line 4555 "parser.y" /* yacc.c:1646  */
    { 
		(yyval.node) = new_node("access");
		Setattr((yyval.node),"kind","protected");
		cplus_mode = CPLUS_PROTECTED;
	      }
#line 8287 "y.tab.c" /* yacc.c:1646  */
    break;

  case 233:
#line 4563 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 8293 "y.tab.c" /* yacc.c:1646  */
    break;

  case 234:
#line 4566 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 8299 "y.tab.c" /* yacc.c:1646  */
    break;

  case 235:
#line 4570 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 8305 "y.tab.c" /* yacc.c:1646  */
    break;

  case 236:
#line 4573 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 8311 "y.tab.c" /* yacc.c:1646  */
    break;

  case 237:
#line 4574 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 8317 "y.tab.c" /* yacc.c:1646  */
    break;

  case 238:
#line 4575 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 8323 "y.tab.c" /* yacc.c:1646  */
    break;

  case 239:
#line 4576 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 8329 "y.tab.c" /* yacc.c:1646  */
    break;

  case 240:
#line 4577 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 8335 "y.tab.c" /* yacc.c:1646  */
    break;

  case 241:
#line 4578 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 8341 "y.tab.c" /* yacc.c:1646  */
    break;

  case 242:
#line 4579 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 8347 "y.tab.c" /* yacc.c:1646  */
    break;

  case 243:
#line 4580 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 8353 "y.tab.c" /* yacc.c:1646  */
    break;

  case 244:
#line 4583 "parser.y" /* yacc.c:1646  */
    {
	            Clear(scanner_ccode);
		    (yyval.dtype).val = 0;
		    (yyval.dtype).throws = (yyvsp[-1].dtype).throws;
		    (yyval.dtype).throwf = (yyvsp[-1].dtype).throwf;
		    (yyval.dtype).nexcept = (yyvsp[-1].dtype).nexcept;
               }
#line 8365 "y.tab.c" /* yacc.c:1646  */
    break;

  case 245:
#line 4590 "parser.y" /* yacc.c:1646  */
    {
	            Clear(scanner_ccode);
		    (yyval.dtype).val = (yyvsp[-1].dtype).val;
		    (yyval.dtype).throws = (yyvsp[-3].dtype).throws;
		    (yyval.dtype).throwf = (yyvsp[-3].dtype).throwf;
		    (yyval.dtype).nexcept = (yyvsp[-3].dtype).nexcept;
               }
#line 8377 "y.tab.c" /* yacc.c:1646  */
    break;

  case 246:
#line 4597 "parser.y" /* yacc.c:1646  */
    { 
		    skip_balanced('{','}'); 
		    (yyval.dtype).val = 0;
		    (yyval.dtype).throws = (yyvsp[-1].dtype).throws;
		    (yyval.dtype).throwf = (yyvsp[-1].dtype).throwf;
		    (yyval.dtype).nexcept = (yyvsp[-1].dtype).nexcept;
	       }
#line 8389 "y.tab.c" /* yacc.c:1646  */
    break;

  case 247:
#line 4606 "parser.y" /* yacc.c:1646  */
    { 
                     Clear(scanner_ccode);
                     (yyval.dtype).val = 0;
                     (yyval.dtype).qualifier = (yyvsp[-1].dtype).qualifier;
                     (yyval.dtype).bitfield = 0;
                     (yyval.dtype).throws = (yyvsp[-1].dtype).throws;
                     (yyval.dtype).throwf = (yyvsp[-1].dtype).throwf;
                     (yyval.dtype).nexcept = (yyvsp[-1].dtype).nexcept;
                }
#line 8403 "y.tab.c" /* yacc.c:1646  */
    break;

  case 248:
#line 4615 "parser.y" /* yacc.c:1646  */
    { 
                     Clear(scanner_ccode);
                     (yyval.dtype).val = (yyvsp[-1].dtype).val;
                     (yyval.dtype).qualifier = (yyvsp[-3].dtype).qualifier;
                     (yyval.dtype).bitfield = 0;
                     (yyval.dtype).throws = (yyvsp[-3].dtype).throws; 
                     (yyval.dtype).throwf = (yyvsp[-3].dtype).throwf; 
                     (yyval.dtype).nexcept = (yyvsp[-3].dtype).nexcept; 
               }
#line 8417 "y.tab.c" /* yacc.c:1646  */
    break;

  case 249:
#line 4624 "parser.y" /* yacc.c:1646  */
    { 
                     skip_balanced('{','}');
                     (yyval.dtype).val = 0;
                     (yyval.dtype).qualifier = (yyvsp[-1].dtype).qualifier;
                     (yyval.dtype).bitfield = 0;
                     (yyval.dtype).throws = (yyvsp[-1].dtype).throws; 
                     (yyval.dtype).throwf = (yyvsp[-1].dtype).throwf; 
                     (yyval.dtype).nexcept = (yyvsp[-1].dtype).nexcept; 
               }
#line 8431 "y.tab.c" /* yacc.c:1646  */
    break;

  case 250:
#line 4636 "parser.y" /* yacc.c:1646  */
    { }
#line 8437 "y.tab.c" /* yacc.c:1646  */
    break;

  case 251:
#line 4639 "parser.y" /* yacc.c:1646  */
    { (yyval.type) = (yyvsp[0].type);
                  /* Printf(stdout,"primitive = '%s'\n", $$);*/
                }
#line 8445 "y.tab.c" /* yacc.c:1646  */
    break;

  case 252:
#line 4642 "parser.y" /* yacc.c:1646  */
    { (yyval.type) = (yyvsp[0].type); }
#line 8451 "y.tab.c" /* yacc.c:1646  */
    break;

  case 253:
#line 4643 "parser.y" /* yacc.c:1646  */
    { (yyval.type) = (yyvsp[0].type); }
#line 8457 "y.tab.c" /* yacc.c:1646  */
    break;

  case 254:
#line 4644 "parser.y" /* yacc.c:1646  */
    { (yyval.type) = NewStringf("%s%s",(yyvsp[-1].type),(yyvsp[0].id)); }
#line 8463 "y.tab.c" /* yacc.c:1646  */
    break;

  case 255:
#line 4645 "parser.y" /* yacc.c:1646  */
    { (yyval.type) = (yyvsp[0].type); }
#line 8469 "y.tab.c" /* yacc.c:1646  */
    break;

  case 256:
#line 4647 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.type) = (yyvsp[0].str);
               }
#line 8477 "y.tab.c" /* yacc.c:1646  */
    break;

  case 257:
#line 4656 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = "extern"; }
#line 8483 "y.tab.c" /* yacc.c:1646  */
    break;

  case 258:
#line 4657 "parser.y" /* yacc.c:1646  */
    {
                   if (strcmp((yyvsp[0].id),"C") == 0) {
		     (yyval.id) = "externc";
                   } else if (strcmp((yyvsp[0].id),"C++") == 0) {
		     (yyval.id) = "extern";
		   } else {
		     Swig_warning(WARN_PARSE_UNDEFINED_EXTERN,cparse_file, cparse_line,"Unrecognized extern type \"%s\".\n", (yyvsp[0].id));
		     (yyval.id) = 0;
		   }
               }
#line 8498 "y.tab.c" /* yacc.c:1646  */
    break;

  case 259:
#line 4667 "parser.y" /* yacc.c:1646  */
    {
                   if (strcmp((yyvsp[-1].id),"C") == 0) {
		     (yyval.id) = "externc thread_local";
                   } else if (strcmp((yyvsp[-1].id),"C++") == 0) {
		     (yyval.id) = "extern thread_local";
		   } else {
		     Swig_warning(WARN_PARSE_UNDEFINED_EXTERN,cparse_file, cparse_line,"Unrecognized extern type \"%s\".\n", (yyvsp[-1].id));
		     (yyval.id) = 0;
		   }
               }
#line 8513 "y.tab.c" /* yacc.c:1646  */
    break;

  case 260:
#line 4677 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = "static"; }
#line 8519 "y.tab.c" /* yacc.c:1646  */
    break;

  case 261:
#line 4678 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = "typedef"; }
#line 8525 "y.tab.c" /* yacc.c:1646  */
    break;

  case 262:
#line 4679 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = "virtual"; }
#line 8531 "y.tab.c" /* yacc.c:1646  */
    break;

  case 263:
#line 4680 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = "friend"; }
#line 8537 "y.tab.c" /* yacc.c:1646  */
    break;

  case 264:
#line 4681 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = "explicit"; }
#line 8543 "y.tab.c" /* yacc.c:1646  */
    break;

  case 265:
#line 4682 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = "constexpr"; }
#line 8549 "y.tab.c" /* yacc.c:1646  */
    break;

  case 266:
#line 4683 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = "explicit constexpr"; }
#line 8555 "y.tab.c" /* yacc.c:1646  */
    break;

  case 267:
#line 4684 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = "explicit constexpr"; }
#line 8561 "y.tab.c" /* yacc.c:1646  */
    break;

  case 268:
#line 4685 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = "static constexpr"; }
#line 8567 "y.tab.c" /* yacc.c:1646  */
    break;

  case 269:
#line 4686 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = "static constexpr"; }
#line 8573 "y.tab.c" /* yacc.c:1646  */
    break;

  case 270:
#line 4687 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = "thread_local"; }
#line 8579 "y.tab.c" /* yacc.c:1646  */
    break;

  case 271:
#line 4688 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = "static thread_local"; }
#line 8585 "y.tab.c" /* yacc.c:1646  */
    break;

  case 272:
#line 4689 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = "static thread_local"; }
#line 8591 "y.tab.c" /* yacc.c:1646  */
    break;

  case 273:
#line 4690 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = "extern thread_local"; }
#line 8597 "y.tab.c" /* yacc.c:1646  */
    break;

  case 274:
#line 4691 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = "extern thread_local"; }
#line 8603 "y.tab.c" /* yacc.c:1646  */
    break;

  case 275:
#line 4692 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = 0; }
#line 8609 "y.tab.c" /* yacc.c:1646  */
    break;

  case 276:
#line 4699 "parser.y" /* yacc.c:1646  */
    {
                 Parm *p;
		 (yyval.pl) = (yyvsp[0].pl);
		 p = (yyvsp[0].pl);
                 while (p) {
		   Replace(Getattr(p,"type"),"typename ", "", DOH_REPLACE_ANY);
		   p = nextSibling(p);
                 }
               }
#line 8623 "y.tab.c" /* yacc.c:1646  */
    break;

  case 277:
#line 4710 "parser.y" /* yacc.c:1646  */
    {
                  set_nextSibling((yyvsp[-1].p),(yyvsp[0].pl));
                  (yyval.pl) = (yyvsp[-1].p);
		}
#line 8632 "y.tab.c" /* yacc.c:1646  */
    break;

  case 278:
#line 4714 "parser.y" /* yacc.c:1646  */
    { (yyval.pl) = 0; }
#line 8638 "y.tab.c" /* yacc.c:1646  */
    break;

  case 279:
#line 4717 "parser.y" /* yacc.c:1646  */
    {
                 set_nextSibling((yyvsp[-1].p),(yyvsp[0].pl));
		 (yyval.pl) = (yyvsp[-1].p);
                }
#line 8647 "y.tab.c" /* yacc.c:1646  */
    break;

  case 280:
#line 4721 "parser.y" /* yacc.c:1646  */
    { (yyval.pl) = 0; }
#line 8653 "y.tab.c" /* yacc.c:1646  */
    break;

  case 281:
#line 4725 "parser.y" /* yacc.c:1646  */
    {
                   SwigType_push((yyvsp[-1].type),(yyvsp[0].decl).type);
		   (yyval.p) = NewParmWithoutFileLineInfo((yyvsp[-1].type),(yyvsp[0].decl).id);
		   Setfile((yyval.p),cparse_file);
		   Setline((yyval.p),cparse_line);
		   if ((yyvsp[0].decl).defarg) {
		     Setattr((yyval.p),"value",(yyvsp[0].decl).defarg);
		   }
		}
#line 8667 "y.tab.c" /* yacc.c:1646  */
    break;

  case 282:
#line 4735 "parser.y" /* yacc.c:1646  */
    {
                  (yyval.p) = NewParmWithoutFileLineInfo(NewStringf("template<class> %s %s", (yyvsp[-2].id),(yyvsp[-1].str)), 0);
		  Setfile((yyval.p),cparse_file);
		  Setline((yyval.p),cparse_line);
                  if ((yyvsp[0].dtype).val) {
                    Setattr((yyval.p),"value",(yyvsp[0].dtype).val);
                  }
                }
#line 8680 "y.tab.c" /* yacc.c:1646  */
    break;

  case 283:
#line 4743 "parser.y" /* yacc.c:1646  */
    {
		  SwigType *t = NewString("v(...)");
		  (yyval.p) = NewParmWithoutFileLineInfo(t, 0);
		  Setfile((yyval.p),cparse_file);
		  Setline((yyval.p),cparse_line);
		}
#line 8691 "y.tab.c" /* yacc.c:1646  */
    break;

  case 284:
#line 4751 "parser.y" /* yacc.c:1646  */
    {
                 Parm *p;
		 (yyval.p) = (yyvsp[0].p);
		 p = (yyvsp[0].p);
                 while (p) {
		   if (Getattr(p,"type")) {
		     Replace(Getattr(p,"type"),"typename ", "", DOH_REPLACE_ANY);
		   }
		   p = nextSibling(p);
                 }
               }
#line 8707 "y.tab.c" /* yacc.c:1646  */
    break;

  case 285:
#line 4764 "parser.y" /* yacc.c:1646  */
    {
                  set_nextSibling((yyvsp[-1].p),(yyvsp[0].p));
                  (yyval.p) = (yyvsp[-1].p);
		}
#line 8716 "y.tab.c" /* yacc.c:1646  */
    break;

  case 286:
#line 4768 "parser.y" /* yacc.c:1646  */
    { (yyval.p) = 0; }
#line 8722 "y.tab.c" /* yacc.c:1646  */
    break;

  case 287:
#line 4771 "parser.y" /* yacc.c:1646  */
    {
                 set_nextSibling((yyvsp[-1].p),(yyvsp[0].p));
		 (yyval.p) = (yyvsp[-1].p);
                }
#line 8731 "y.tab.c" /* yacc.c:1646  */
    break;

  case 288:
#line 4775 "parser.y" /* yacc.c:1646  */
    { (yyval.p) = 0; }
#line 8737 "y.tab.c" /* yacc.c:1646  */
    break;

  case 289:
#line 4779 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.p) = (yyvsp[0].p);
		  {
		    /* We need to make a possible adjustment for integer parameters. */
		    SwigType *type;
		    Node     *n = 0;

		    while (!n) {
		      type = Getattr((yyvsp[0].p),"type");
		      n = Swig_symbol_clookup(type,0);     /* See if we can find a node that matches the typename */
		      if ((n) && (Strcmp(nodeType(n),"cdecl") == 0)) {
			SwigType *decl = Getattr(n,"decl");
			if (!SwigType_isfunction(decl)) {
			  String *value = Getattr(n,"value");
			  if (value) {
			    String *v = Copy(value);
			    Setattr((yyvsp[0].p),"type",v);
			    Delete(v);
			    n = 0;
			  }
			}
		      } else {
			break;
		      }
		    }
		  }

               }
#line 8770 "y.tab.c" /* yacc.c:1646  */
    break;

  case 290:
#line 4807 "parser.y" /* yacc.c:1646  */
    {
                  (yyval.p) = NewParmWithoutFileLineInfo(0,0);
                  Setfile((yyval.p),cparse_file);
		  Setline((yyval.p),cparse_line);
		  Setattr((yyval.p),"value",(yyvsp[0].dtype).val);
               }
#line 8781 "y.tab.c" /* yacc.c:1646  */
    break;

  case 291:
#line 4815 "parser.y" /* yacc.c:1646  */
    { 
                  (yyval.dtype) = (yyvsp[0].dtype); 
		  if ((yyvsp[0].dtype).type == T_ERROR) {
		    Swig_warning(WARN_PARSE_BAD_DEFAULT,cparse_file, cparse_line, "Can't set default argument (ignored)\n");
		    (yyval.dtype).val = 0;
		    (yyval.dtype).rawval = 0;
		    (yyval.dtype).bitfield = 0;
		    (yyval.dtype).throws = 0;
		    (yyval.dtype).throwf = 0;
		    (yyval.dtype).nexcept = 0;
		  }
               }
#line 8798 "y.tab.c" /* yacc.c:1646  */
    break;

  case 292:
#line 4827 "parser.y" /* yacc.c:1646  */
    { 
		  (yyval.dtype) = (yyvsp[-3].dtype);
		  if ((yyvsp[-3].dtype).type == T_ERROR) {
		    Swig_warning(WARN_PARSE_BAD_DEFAULT,cparse_file, cparse_line, "Can't set default argument (ignored)\n");
		    (yyval.dtype) = (yyvsp[-3].dtype);
		    (yyval.dtype).val = 0;
		    (yyval.dtype).rawval = 0;
		    (yyval.dtype).bitfield = 0;
		    (yyval.dtype).throws = 0;
		    (yyval.dtype).throwf = 0;
		    (yyval.dtype).nexcept = 0;
		  } else {
		    (yyval.dtype).val = NewStringf("%s[%s]",(yyvsp[-3].dtype).val,(yyvsp[-1].dtype).val); 
		  }		  
               }
#line 8818 "y.tab.c" /* yacc.c:1646  */
    break;

  case 293:
#line 4842 "parser.y" /* yacc.c:1646  */
    {
		 skip_balanced('{','}');
		 (yyval.dtype).val = NewString(scanner_ccode);
		 (yyval.dtype).rawval = 0;
                 (yyval.dtype).type = T_INT;
		 (yyval.dtype).bitfield = 0;
		 (yyval.dtype).throws = 0;
		 (yyval.dtype).throwf = 0;
		 (yyval.dtype).nexcept = 0;
	       }
#line 8833 "y.tab.c" /* yacc.c:1646  */
    break;

  case 294:
#line 4852 "parser.y" /* yacc.c:1646  */
    { 
		 (yyval.dtype).val = 0;
		 (yyval.dtype).rawval = 0;
		 (yyval.dtype).type = 0;
		 (yyval.dtype).bitfield = (yyvsp[0].dtype).val;
		 (yyval.dtype).throws = 0;
		 (yyval.dtype).throwf = 0;
		 (yyval.dtype).nexcept = 0;
	       }
#line 8847 "y.tab.c" /* yacc.c:1646  */
    break;

  case 295:
#line 4861 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.dtype).val = 0;
                 (yyval.dtype).rawval = 0;
                 (yyval.dtype).type = T_INT;
		 (yyval.dtype).bitfield = 0;
		 (yyval.dtype).throws = 0;
		 (yyval.dtype).throwf = 0;
		 (yyval.dtype).nexcept = 0;
               }
#line 8861 "y.tab.c" /* yacc.c:1646  */
    break;

  case 296:
#line 4872 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.decl) = (yyvsp[-1].decl);
		 (yyval.decl).defarg = (yyvsp[0].dtype).rawval ? (yyvsp[0].dtype).rawval : (yyvsp[0].dtype).val;
            }
#line 8870 "y.tab.c" /* yacc.c:1646  */
    break;

  case 297:
#line 4876 "parser.y" /* yacc.c:1646  */
    {
              (yyval.decl) = (yyvsp[-1].decl);
	      (yyval.decl).defarg = (yyvsp[0].dtype).rawval ? (yyvsp[0].dtype).rawval : (yyvsp[0].dtype).val;
            }
#line 8879 "y.tab.c" /* yacc.c:1646  */
    break;

  case 298:
#line 4880 "parser.y" /* yacc.c:1646  */
    {
   	      (yyval.decl).type = 0;
              (yyval.decl).id = 0;
	      (yyval.decl).defarg = (yyvsp[0].dtype).rawval ? (yyvsp[0].dtype).rawval : (yyvsp[0].dtype).val;
            }
#line 8889 "y.tab.c" /* yacc.c:1646  */
    break;

  case 299:
#line 4887 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.decl) = (yyvsp[0].decl);
		 if (SwigType_isfunction((yyvsp[0].decl).type)) {
		   Delete(SwigType_pop_function((yyvsp[0].decl).type));
		 } else if (SwigType_isarray((yyvsp[0].decl).type)) {
		   SwigType *ta = SwigType_pop_arrays((yyvsp[0].decl).type);
		   if (SwigType_isfunction((yyvsp[0].decl).type)) {
		     Delete(SwigType_pop_function((yyvsp[0].decl).type));
		   } else {
		     (yyval.decl).parms = 0;
		   }
		   SwigType_push((yyvsp[0].decl).type,ta);
		   Delete(ta);
		 } else {
		   (yyval.decl).parms = 0;
		 }
            }
#line 8911 "y.tab.c" /* yacc.c:1646  */
    break;

  case 300:
#line 4904 "parser.y" /* yacc.c:1646  */
    {
              (yyval.decl) = (yyvsp[0].decl);
	      if (SwigType_isfunction((yyvsp[0].decl).type)) {
		Delete(SwigType_pop_function((yyvsp[0].decl).type));
	      } else if (SwigType_isarray((yyvsp[0].decl).type)) {
		SwigType *ta = SwigType_pop_arrays((yyvsp[0].decl).type);
		if (SwigType_isfunction((yyvsp[0].decl).type)) {
		  Delete(SwigType_pop_function((yyvsp[0].decl).type));
		} else {
		  (yyval.decl).parms = 0;
		}
		SwigType_push((yyvsp[0].decl).type,ta);
		Delete(ta);
	      } else {
		(yyval.decl).parms = 0;
	      }
            }
#line 8933 "y.tab.c" /* yacc.c:1646  */
    break;

  case 301:
#line 4921 "parser.y" /* yacc.c:1646  */
    {
   	      (yyval.decl).type = 0;
              (yyval.decl).id = 0;
	      (yyval.decl).parms = 0;
	      }
#line 8943 "y.tab.c" /* yacc.c:1646  */
    break;

  case 302:
#line 4929 "parser.y" /* yacc.c:1646  */
    {
              (yyval.decl) = (yyvsp[0].decl);
	      if ((yyval.decl).type) {
		SwigType_push((yyvsp[-1].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[-1].type);
           }
#line 8956 "y.tab.c" /* yacc.c:1646  */
    break;

  case 303:
#line 4937 "parser.y" /* yacc.c:1646  */
    {
              (yyval.decl) = (yyvsp[0].decl);
	      SwigType_add_reference((yyvsp[-2].type));
              if ((yyval.decl).type) {
		SwigType_push((yyvsp[-2].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[-2].type);
           }
#line 8970 "y.tab.c" /* yacc.c:1646  */
    break;

  case 304:
#line 4946 "parser.y" /* yacc.c:1646  */
    {
              (yyval.decl) = (yyvsp[0].decl);
	      SwigType_add_rvalue_reference((yyvsp[-2].type));
              if ((yyval.decl).type) {
		SwigType_push((yyvsp[-2].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[-2].type);
           }
#line 8984 "y.tab.c" /* yacc.c:1646  */
    break;

  case 305:
#line 4955 "parser.y" /* yacc.c:1646  */
    {
              (yyval.decl) = (yyvsp[0].decl);
	      if (!(yyval.decl).type) (yyval.decl).type = NewStringEmpty();
           }
#line 8993 "y.tab.c" /* yacc.c:1646  */
    break;

  case 306:
#line 4959 "parser.y" /* yacc.c:1646  */
    {
	     (yyval.decl) = (yyvsp[0].decl);
	     (yyval.decl).type = NewStringEmpty();
	     SwigType_add_reference((yyval.decl).type);
	     if ((yyvsp[0].decl).type) {
	       SwigType_push((yyval.decl).type,(yyvsp[0].decl).type);
	       Delete((yyvsp[0].decl).type);
	     }
           }
#line 9007 "y.tab.c" /* yacc.c:1646  */
    break;

  case 307:
#line 4968 "parser.y" /* yacc.c:1646  */
    {
	     /* Introduced in C++11, move operator && */
             /* Adds one S/R conflict */
	     (yyval.decl) = (yyvsp[0].decl);
	     (yyval.decl).type = NewStringEmpty();
	     SwigType_add_rvalue_reference((yyval.decl).type);
	     if ((yyvsp[0].decl).type) {
	       SwigType_push((yyval.decl).type,(yyvsp[0].decl).type);
	       Delete((yyvsp[0].decl).type);
	     }
           }
#line 9023 "y.tab.c" /* yacc.c:1646  */
    break;

  case 308:
#line 4979 "parser.y" /* yacc.c:1646  */
    { 
	     SwigType *t = NewStringEmpty();

	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-2].str));
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = t;
	     }
#line 9039 "y.tab.c" /* yacc.c:1646  */
    break;

  case 309:
#line 4990 "parser.y" /* yacc.c:1646  */
    { 
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-2].str));
	     SwigType_push((yyvsp[-3].type),t);
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[-3].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[-3].type);
	     Delete(t);
	   }
#line 9056 "y.tab.c" /* yacc.c:1646  */
    break;

  case 310:
#line 5002 "parser.y" /* yacc.c:1646  */
    { 
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer((yyvsp[-4].type),(yyvsp[-3].str));
	     SwigType_add_reference((yyvsp[-4].type));
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[-4].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[-4].type);
	   }
#line 9071 "y.tab.c" /* yacc.c:1646  */
    break;

  case 311:
#line 5012 "parser.y" /* yacc.c:1646  */
    { 
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-3].str));
	     SwigType_add_reference(t);
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     } 
	     (yyval.decl).type = t;
	   }
#line 9087 "y.tab.c" /* yacc.c:1646  */
    break;

  case 312:
#line 5026 "parser.y" /* yacc.c:1646  */
    {
              (yyval.decl) = (yyvsp[0].decl);
	      if ((yyval.decl).type) {
		SwigType_push((yyvsp[-4].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[-4].type);
           }
#line 9100 "y.tab.c" /* yacc.c:1646  */
    break;

  case 313:
#line 5034 "parser.y" /* yacc.c:1646  */
    {
              (yyval.decl) = (yyvsp[0].decl);
	      SwigType_add_reference((yyvsp[-5].type));
              if ((yyval.decl).type) {
		SwigType_push((yyvsp[-5].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[-5].type);
           }
#line 9114 "y.tab.c" /* yacc.c:1646  */
    break;

  case 314:
#line 5043 "parser.y" /* yacc.c:1646  */
    {
              (yyval.decl) = (yyvsp[0].decl);
	      SwigType_add_rvalue_reference((yyvsp[-5].type));
              if ((yyval.decl).type) {
		SwigType_push((yyvsp[-5].type),(yyval.decl).type);
		Delete((yyval.decl).type);
	      }
	      (yyval.decl).type = (yyvsp[-5].type);
           }
#line 9128 "y.tab.c" /* yacc.c:1646  */
    break;

  case 315:
#line 5052 "parser.y" /* yacc.c:1646  */
    {
              (yyval.decl) = (yyvsp[0].decl);
	      if (!(yyval.decl).type) (yyval.decl).type = NewStringEmpty();
           }
#line 9137 "y.tab.c" /* yacc.c:1646  */
    break;

  case 316:
#line 5056 "parser.y" /* yacc.c:1646  */
    {
	     (yyval.decl) = (yyvsp[0].decl);
	     (yyval.decl).type = NewStringEmpty();
	     SwigType_add_reference((yyval.decl).type);
	     if ((yyvsp[0].decl).type) {
	       SwigType_push((yyval.decl).type,(yyvsp[0].decl).type);
	       Delete((yyvsp[0].decl).type);
	     }
           }
#line 9151 "y.tab.c" /* yacc.c:1646  */
    break;

  case 317:
#line 5065 "parser.y" /* yacc.c:1646  */
    {
	     /* Introduced in C++11, move operator && */
             /* Adds one S/R conflict */
	     (yyval.decl) = (yyvsp[0].decl);
	     (yyval.decl).type = NewStringEmpty();
	     SwigType_add_rvalue_reference((yyval.decl).type);
	     if ((yyvsp[0].decl).type) {
	       SwigType_push((yyval.decl).type,(yyvsp[0].decl).type);
	       Delete((yyvsp[0].decl).type);
	     }
           }
#line 9167 "y.tab.c" /* yacc.c:1646  */
    break;

  case 318:
#line 5076 "parser.y" /* yacc.c:1646  */
    { 
	     SwigType *t = NewStringEmpty();

	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-5].str));
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = t;
	     }
#line 9183 "y.tab.c" /* yacc.c:1646  */
    break;

  case 319:
#line 5087 "parser.y" /* yacc.c:1646  */
    { 
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-5].str));
	     SwigType_push((yyvsp[-6].type),t);
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[-6].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[-6].type);
	     Delete(t);
	   }
#line 9200 "y.tab.c" /* yacc.c:1646  */
    break;

  case 320:
#line 5099 "parser.y" /* yacc.c:1646  */
    { 
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer((yyvsp[-7].type),(yyvsp[-6].str));
	     SwigType_add_reference((yyvsp[-7].type));
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[-7].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[-7].type);
	   }
#line 9215 "y.tab.c" /* yacc.c:1646  */
    break;

  case 321:
#line 5109 "parser.y" /* yacc.c:1646  */
    { 
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer((yyvsp[-7].type),(yyvsp[-6].str));
	     SwigType_add_rvalue_reference((yyvsp[-7].type));
	     if ((yyval.decl).type) {
	       SwigType_push((yyvsp[-7].type),(yyval.decl).type);
	       Delete((yyval.decl).type);
	     }
	     (yyval.decl).type = (yyvsp[-7].type);
	   }
#line 9230 "y.tab.c" /* yacc.c:1646  */
    break;

  case 322:
#line 5119 "parser.y" /* yacc.c:1646  */
    { 
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-6].str));
	     SwigType_add_reference(t);
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     } 
	     (yyval.decl).type = t;
	   }
#line 9246 "y.tab.c" /* yacc.c:1646  */
    break;

  case 323:
#line 5130 "parser.y" /* yacc.c:1646  */
    { 
	     SwigType *t = NewStringEmpty();
	     (yyval.decl) = (yyvsp[0].decl);
	     SwigType_add_memberpointer(t,(yyvsp[-6].str));
	     SwigType_add_rvalue_reference(t);
	     if ((yyval.decl).type) {
	       SwigType_push(t,(yyval.decl).type);
	       Delete((yyval.decl).type);
	     } 
	     (yyval.decl).type = t;
	   }
#line 9262 "y.tab.c" /* yacc.c:1646  */
    break;

  case 324:
#line 5143 "parser.y" /* yacc.c:1646  */
    {
                /* Note: This is non-standard C.  Template declarator is allowed to follow an identifier */
                 (yyval.decl).id = Char((yyvsp[0].str));
		 (yyval.decl).type = 0;
		 (yyval.decl).parms = 0;
		 (yyval.decl).have_parms = 0;
                  }
#line 9274 "y.tab.c" /* yacc.c:1646  */
    break;

  case 325:
#line 5150 "parser.y" /* yacc.c:1646  */
    {
                  (yyval.decl).id = Char(NewStringf("~%s",(yyvsp[0].str)));
                  (yyval.decl).type = 0;
                  (yyval.decl).parms = 0;
                  (yyval.decl).have_parms = 0;
                  }
#line 9285 "y.tab.c" /* yacc.c:1646  */
    break;

  case 326:
#line 5158 "parser.y" /* yacc.c:1646  */
    {
                  (yyval.decl).id = Char((yyvsp[-1].str));
                  (yyval.decl).type = 0;
                  (yyval.decl).parms = 0;
                  (yyval.decl).have_parms = 0;
                  }
#line 9296 "y.tab.c" /* yacc.c:1646  */
    break;

  case 327:
#line 5174 "parser.y" /* yacc.c:1646  */
    {
		    (yyval.decl) = (yyvsp[-1].decl);
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[-2].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[-2].type);
                  }
#line 9309 "y.tab.c" /* yacc.c:1646  */
    break;

  case 328:
#line 5182 "parser.y" /* yacc.c:1646  */
    {
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-1].decl);
		    t = NewStringEmpty();
		    SwigType_add_memberpointer(t,(yyvsp[-3].str));
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
		    }
#line 9325 "y.tab.c" /* yacc.c:1646  */
    break;

  case 329:
#line 5193 "parser.y" /* yacc.c:1646  */
    { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-2].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,"");
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
#line 9341 "y.tab.c" /* yacc.c:1646  */
    break;

  case 330:
#line 5204 "parser.y" /* yacc.c:1646  */
    { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-3].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,(yyvsp[-1].dtype).val);
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
#line 9357 "y.tab.c" /* yacc.c:1646  */
    break;

  case 331:
#line 5215 "parser.y" /* yacc.c:1646  */
    {
		    SwigType *t;
                    (yyval.decl) = (yyvsp[-3].decl);
		    t = NewStringEmpty();
		    SwigType_add_function(t,(yyvsp[-1].pl));
		    if (!(yyval.decl).have_parms) {
		      (yyval.decl).parms = (yyvsp[-1].pl);
		      (yyval.decl).have_parms = 1;
		    }
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = t;
		    } else {
		      SwigType_push(t, (yyval.decl).type);
		      Delete((yyval.decl).type);
		      (yyval.decl).type = t;
		    }
		  }
#line 9379 "y.tab.c" /* yacc.c:1646  */
    break;

  case 332:
#line 5234 "parser.y" /* yacc.c:1646  */
    {
                /* Note: This is non-standard C.  Template declarator is allowed to follow an identifier */
                 (yyval.decl).id = Char((yyvsp[0].str));
		 (yyval.decl).type = 0;
		 (yyval.decl).parms = 0;
		 (yyval.decl).have_parms = 0;
                  }
#line 9391 "y.tab.c" /* yacc.c:1646  */
    break;

  case 333:
#line 5242 "parser.y" /* yacc.c:1646  */
    {
                  (yyval.decl).id = Char(NewStringf("~%s",(yyvsp[0].str)));
                  (yyval.decl).type = 0;
                  (yyval.decl).parms = 0;
                  (yyval.decl).have_parms = 0;
                  }
#line 9402 "y.tab.c" /* yacc.c:1646  */
    break;

  case 334:
#line 5259 "parser.y" /* yacc.c:1646  */
    {
		    (yyval.decl) = (yyvsp[-1].decl);
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[-2].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[-2].type);
                  }
#line 9415 "y.tab.c" /* yacc.c:1646  */
    break;

  case 335:
#line 5267 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.decl) = (yyvsp[-1].decl);
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = NewStringEmpty();
		    }
		    SwigType_add_reference((yyval.decl).type);
                  }
#line 9427 "y.tab.c" /* yacc.c:1646  */
    break;

  case 336:
#line 5274 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.decl) = (yyvsp[-1].decl);
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = NewStringEmpty();
		    }
		    SwigType_add_rvalue_reference((yyval.decl).type);
                  }
#line 9439 "y.tab.c" /* yacc.c:1646  */
    break;

  case 337:
#line 5281 "parser.y" /* yacc.c:1646  */
    {
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-1].decl);
		    t = NewStringEmpty();
		    SwigType_add_memberpointer(t,(yyvsp[-3].str));
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
		    }
#line 9455 "y.tab.c" /* yacc.c:1646  */
    break;

  case 338:
#line 5292 "parser.y" /* yacc.c:1646  */
    { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-2].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,"");
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
#line 9471 "y.tab.c" /* yacc.c:1646  */
    break;

  case 339:
#line 5303 "parser.y" /* yacc.c:1646  */
    { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-3].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,(yyvsp[-1].dtype).val);
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
#line 9487 "y.tab.c" /* yacc.c:1646  */
    break;

  case 340:
#line 5314 "parser.y" /* yacc.c:1646  */
    {
		    SwigType *t;
                    (yyval.decl) = (yyvsp[-3].decl);
		    t = NewStringEmpty();
		    SwigType_add_function(t,(yyvsp[-1].pl));
		    if (!(yyval.decl).have_parms) {
		      (yyval.decl).parms = (yyvsp[-1].pl);
		      (yyval.decl).have_parms = 1;
		    }
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = t;
		    } else {
		      SwigType_push(t, (yyval.decl).type);
		      Delete((yyval.decl).type);
		      (yyval.decl).type = t;
		    }
                 }
#line 9509 "y.tab.c" /* yacc.c:1646  */
    break;

  case 341:
#line 5334 "parser.y" /* yacc.c:1646  */
    {
		    SwigType *t;
                    Append((yyvsp[-4].str), " "); /* intervening space is mandatory */
                    Append((yyvsp[-4].str), Char((yyvsp[-3].id)));
		    (yyval.decl).id = Char((yyvsp[-4].str));
		    t = NewStringEmpty();
		    SwigType_add_function(t,(yyvsp[-1].pl));
		    if (!(yyval.decl).have_parms) {
		      (yyval.decl).parms = (yyvsp[-1].pl);
		      (yyval.decl).have_parms = 1;
		    }
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = t;
		    } else {
		      SwigType_push(t, (yyval.decl).type);
		      Delete((yyval.decl).type);
		      (yyval.decl).type = t;
		    }
		  }
#line 9533 "y.tab.c" /* yacc.c:1646  */
    break;

  case 342:
#line 5355 "parser.y" /* yacc.c:1646  */
    {
		    (yyval.decl).type = (yyvsp[0].type);
                    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
                  }
#line 9544 "y.tab.c" /* yacc.c:1646  */
    break;

  case 343:
#line 5361 "parser.y" /* yacc.c:1646  */
    { 
                     (yyval.decl) = (yyvsp[0].decl);
                     SwigType_push((yyvsp[-1].type),(yyvsp[0].decl).type);
		     (yyval.decl).type = (yyvsp[-1].type);
		     Delete((yyvsp[0].decl).type);
                  }
#line 9555 "y.tab.c" /* yacc.c:1646  */
    break;

  case 344:
#line 5367 "parser.y" /* yacc.c:1646  */
    {
		    (yyval.decl).type = (yyvsp[-1].type);
		    SwigType_add_reference((yyval.decl).type);
		    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
		  }
#line 9567 "y.tab.c" /* yacc.c:1646  */
    break;

  case 345:
#line 5374 "parser.y" /* yacc.c:1646  */
    {
		    (yyval.decl).type = (yyvsp[-1].type);
		    SwigType_add_rvalue_reference((yyval.decl).type);
		    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
		  }
#line 9579 "y.tab.c" /* yacc.c:1646  */
    break;

  case 346:
#line 5381 "parser.y" /* yacc.c:1646  */
    {
		    (yyval.decl) = (yyvsp[0].decl);
		    SwigType_add_reference((yyvsp[-2].type));
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[-2].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[-2].type);
                  }
#line 9593 "y.tab.c" /* yacc.c:1646  */
    break;

  case 347:
#line 5390 "parser.y" /* yacc.c:1646  */
    {
		    (yyval.decl) = (yyvsp[0].decl);
		    SwigType_add_rvalue_reference((yyvsp[-2].type));
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[-2].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[-2].type);
                  }
#line 9607 "y.tab.c" /* yacc.c:1646  */
    break;

  case 348:
#line 5399 "parser.y" /* yacc.c:1646  */
    {
		    (yyval.decl) = (yyvsp[0].decl);
                  }
#line 9615 "y.tab.c" /* yacc.c:1646  */
    break;

  case 349:
#line 5402 "parser.y" /* yacc.c:1646  */
    {
		    (yyval.decl) = (yyvsp[0].decl);
		    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_reference((yyval.decl).type);
		    if ((yyvsp[0].decl).type) {
		      SwigType_push((yyval.decl).type,(yyvsp[0].decl).type);
		      Delete((yyvsp[0].decl).type);
		    }
                  }
#line 9629 "y.tab.c" /* yacc.c:1646  */
    break;

  case 350:
#line 5411 "parser.y" /* yacc.c:1646  */
    {
		    (yyval.decl) = (yyvsp[0].decl);
		    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_rvalue_reference((yyval.decl).type);
		    if ((yyvsp[0].decl).type) {
		      SwigType_push((yyval.decl).type,(yyvsp[0].decl).type);
		      Delete((yyvsp[0].decl).type);
		    }
                  }
#line 9643 "y.tab.c" /* yacc.c:1646  */
    break;

  case 351:
#line 5420 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.decl).id = 0;
                    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
                    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_reference((yyval.decl).type);
                  }
#line 9655 "y.tab.c" /* yacc.c:1646  */
    break;

  case 352:
#line 5427 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.decl).id = 0;
                    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
                    (yyval.decl).type = NewStringEmpty();
		    SwigType_add_rvalue_reference((yyval.decl).type);
                  }
#line 9667 "y.tab.c" /* yacc.c:1646  */
    break;

  case 353:
#line 5434 "parser.y" /* yacc.c:1646  */
    { 
		    (yyval.decl).type = NewStringEmpty();
                    SwigType_add_memberpointer((yyval.decl).type,(yyvsp[-1].str));
                    (yyval.decl).id = 0;
                    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
      	          }
#line 9679 "y.tab.c" /* yacc.c:1646  */
    break;

  case 354:
#line 5441 "parser.y" /* yacc.c:1646  */
    { 
		    SwigType *t = NewStringEmpty();
                    (yyval.decl).type = (yyvsp[-2].type);
		    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
		    SwigType_add_memberpointer(t,(yyvsp[-1].str));
		    SwigType_push((yyval.decl).type,t);
		    Delete(t);
                  }
#line 9694 "y.tab.c" /* yacc.c:1646  */
    break;

  case 355:
#line 5451 "parser.y" /* yacc.c:1646  */
    { 
		    (yyval.decl) = (yyvsp[0].decl);
		    SwigType_add_memberpointer((yyvsp[-3].type),(yyvsp[-2].str));
		    if ((yyval.decl).type) {
		      SwigType_push((yyvsp[-3].type),(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = (yyvsp[-3].type);
                  }
#line 9708 "y.tab.c" /* yacc.c:1646  */
    break;

  case 356:
#line 5462 "parser.y" /* yacc.c:1646  */
    { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-2].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,"");
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
#line 9724 "y.tab.c" /* yacc.c:1646  */
    break;

  case 357:
#line 5473 "parser.y" /* yacc.c:1646  */
    { 
		    SwigType *t;
		    (yyval.decl) = (yyvsp[-3].decl);
		    t = NewStringEmpty();
		    SwigType_add_array(t,(yyvsp[-1].dtype).val);
		    if ((yyval.decl).type) {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		    }
		    (yyval.decl).type = t;
                  }
#line 9740 "y.tab.c" /* yacc.c:1646  */
    break;

  case 358:
#line 5484 "parser.y" /* yacc.c:1646  */
    { 
		    (yyval.decl).type = NewStringEmpty();
		    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
		    SwigType_add_array((yyval.decl).type,"");
                  }
#line 9752 "y.tab.c" /* yacc.c:1646  */
    break;

  case 359:
#line 5491 "parser.y" /* yacc.c:1646  */
    { 
		    (yyval.decl).type = NewStringEmpty();
		    (yyval.decl).id = 0;
		    (yyval.decl).parms = 0;
		    (yyval.decl).have_parms = 0;
		    SwigType_add_array((yyval.decl).type,(yyvsp[-1].dtype).val);
		  }
#line 9764 "y.tab.c" /* yacc.c:1646  */
    break;

  case 360:
#line 5498 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.decl) = (yyvsp[-1].decl);
		  }
#line 9772 "y.tab.c" /* yacc.c:1646  */
    break;

  case 361:
#line 5501 "parser.y" /* yacc.c:1646  */
    {
		    SwigType *t;
                    (yyval.decl) = (yyvsp[-3].decl);
		    t = NewStringEmpty();
                    SwigType_add_function(t,(yyvsp[-1].pl));
		    if (!(yyval.decl).type) {
		      (yyval.decl).type = t;
		    } else {
		      SwigType_push(t,(yyval.decl).type);
		      Delete((yyval.decl).type);
		      (yyval.decl).type = t;
		    }
		    if (!(yyval.decl).have_parms) {
		      (yyval.decl).parms = (yyvsp[-1].pl);
		      (yyval.decl).have_parms = 1;
		    }
		  }
#line 9794 "y.tab.c" /* yacc.c:1646  */
    break;

  case 362:
#line 5518 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.decl).type = NewStringEmpty();
                    SwigType_add_function((yyval.decl).type,(yyvsp[-1].pl));
		    (yyval.decl).parms = (yyvsp[-1].pl);
		    (yyval.decl).have_parms = 1;
		    (yyval.decl).id = 0;
                  }
#line 9806 "y.tab.c" /* yacc.c:1646  */
    break;

  case 363:
#line 5528 "parser.y" /* yacc.c:1646  */
    { 
             (yyval.type) = NewStringEmpty();
             SwigType_add_pointer((yyval.type));
	     SwigType_push((yyval.type),(yyvsp[-1].str));
	     SwigType_push((yyval.type),(yyvsp[0].type));
	     Delete((yyvsp[0].type));
           }
#line 9818 "y.tab.c" /* yacc.c:1646  */
    break;

  case 364:
#line 5535 "parser.y" /* yacc.c:1646  */
    {
	     (yyval.type) = NewStringEmpty();
	     SwigType_add_pointer((yyval.type));
	     SwigType_push((yyval.type),(yyvsp[0].type));
	     Delete((yyvsp[0].type));
	   }
#line 9829 "y.tab.c" /* yacc.c:1646  */
    break;

  case 365:
#line 5541 "parser.y" /* yacc.c:1646  */
    { 
	     (yyval.type) = NewStringEmpty();
	     SwigType_add_pointer((yyval.type));
	     SwigType_push((yyval.type),(yyvsp[0].str));
           }
#line 9839 "y.tab.c" /* yacc.c:1646  */
    break;

  case 366:
#line 5546 "parser.y" /* yacc.c:1646  */
    {
	     (yyval.type) = NewStringEmpty();
	     SwigType_add_pointer((yyval.type));
           }
#line 9848 "y.tab.c" /* yacc.c:1646  */
    break;

  case 367:
#line 5552 "parser.y" /* yacc.c:1646  */
    {
	          (yyval.str) = NewStringEmpty();
	          if ((yyvsp[0].id)) SwigType_add_qualifier((yyval.str),(yyvsp[0].id));
               }
#line 9857 "y.tab.c" /* yacc.c:1646  */
    break;

  case 368:
#line 5556 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.str) = (yyvsp[0].str);
	          if ((yyvsp[-1].id)) SwigType_add_qualifier((yyval.str),(yyvsp[-1].id));
               }
#line 9866 "y.tab.c" /* yacc.c:1646  */
    break;

  case 369:
#line 5562 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = "const"; }
#line 9872 "y.tab.c" /* yacc.c:1646  */
    break;

  case 370:
#line 5563 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = "volatile"; }
#line 9878 "y.tab.c" /* yacc.c:1646  */
    break;

  case 371:
#line 5564 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = 0; }
#line 9884 "y.tab.c" /* yacc.c:1646  */
    break;

  case 372:
#line 5570 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.type) = (yyvsp[0].type);
                   Replace((yyval.type),"typename ","", DOH_REPLACE_ANY);
                }
#line 9893 "y.tab.c" /* yacc.c:1646  */
    break;

  case 373:
#line 5576 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.type) = (yyvsp[0].type);
	           SwigType_push((yyval.type),(yyvsp[-1].str));
               }
#line 9902 "y.tab.c" /* yacc.c:1646  */
    break;

  case 374:
#line 5580 "parser.y" /* yacc.c:1646  */
    { (yyval.type) = (yyvsp[0].type); }
#line 9908 "y.tab.c" /* yacc.c:1646  */
    break;

  case 375:
#line 5581 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.type) = (yyvsp[-1].type);
	          SwigType_push((yyval.type),(yyvsp[0].str));
	       }
#line 9917 "y.tab.c" /* yacc.c:1646  */
    break;

  case 376:
#line 5585 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.type) = (yyvsp[-1].type);
	          SwigType_push((yyval.type),(yyvsp[0].str));
	          SwigType_push((yyval.type),(yyvsp[-2].str));
	       }
#line 9927 "y.tab.c" /* yacc.c:1646  */
    break;

  case 377:
#line 5592 "parser.y" /* yacc.c:1646  */
    { (yyval.type) = (yyvsp[0].type);
                  /* Printf(stdout,"primitive = '%s'\n", $$);*/
               }
#line 9935 "y.tab.c" /* yacc.c:1646  */
    break;

  case 378:
#line 5595 "parser.y" /* yacc.c:1646  */
    { (yyval.type) = (yyvsp[0].type); }
#line 9941 "y.tab.c" /* yacc.c:1646  */
    break;

  case 379:
#line 5596 "parser.y" /* yacc.c:1646  */
    { (yyval.type) = (yyvsp[0].type); }
#line 9947 "y.tab.c" /* yacc.c:1646  */
    break;

  case 380:
#line 5597 "parser.y" /* yacc.c:1646  */
    { (yyval.type) = NewStringf("%s%s",(yyvsp[-1].type),(yyvsp[0].id)); }
#line 9953 "y.tab.c" /* yacc.c:1646  */
    break;

  case 381:
#line 5598 "parser.y" /* yacc.c:1646  */
    { (yyval.type) = NewStringf("enum %s", (yyvsp[0].str)); }
#line 9959 "y.tab.c" /* yacc.c:1646  */
    break;

  case 382:
#line 5599 "parser.y" /* yacc.c:1646  */
    { (yyval.type) = (yyvsp[0].type); }
#line 9965 "y.tab.c" /* yacc.c:1646  */
    break;

  case 383:
#line 5601 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.type) = (yyvsp[0].str);
               }
#line 9973 "y.tab.c" /* yacc.c:1646  */
    break;

  case 384:
#line 5604 "parser.y" /* yacc.c:1646  */
    { 
		 (yyval.type) = NewStringf("%s %s", (yyvsp[-1].id), (yyvsp[0].str));
               }
#line 9981 "y.tab.c" /* yacc.c:1646  */
    break;

  case 385:
#line 5607 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.type) = (yyvsp[0].type);
               }
#line 9989 "y.tab.c" /* yacc.c:1646  */
    break;

  case 386:
#line 5612 "parser.y" /* yacc.c:1646  */
    {
                 Node *n = Swig_symbol_clookup((yyvsp[-1].str),0);
                 if (!n) {
		   Swig_error(cparse_file, cparse_line, "Identifier %s not defined.\n", (yyvsp[-1].str));
                   (yyval.type) = (yyvsp[-1].str);
                 } else {
                   (yyval.type) = Getattr(n, "type");
                 }
               }
#line 10003 "y.tab.c" /* yacc.c:1646  */
    break;

  case 387:
#line 5623 "parser.y" /* yacc.c:1646  */
    {
		 if (!(yyvsp[0].ptype).type) (yyvsp[0].ptype).type = NewString("int");
		 if ((yyvsp[0].ptype).us) {
		   (yyval.type) = NewStringf("%s %s", (yyvsp[0].ptype).us, (yyvsp[0].ptype).type);
		   Delete((yyvsp[0].ptype).us);
                   Delete((yyvsp[0].ptype).type);
		 } else {
                   (yyval.type) = (yyvsp[0].ptype).type;
		 }
		 if (Cmp((yyval.type),"signed int") == 0) {
		   Delete((yyval.type));
		   (yyval.type) = NewString("int");
                 } else if (Cmp((yyval.type),"signed long") == 0) {
		   Delete((yyval.type));
                   (yyval.type) = NewString("long");
                 } else if (Cmp((yyval.type),"signed short") == 0) {
		   Delete((yyval.type));
		   (yyval.type) = NewString("short");
		 } else if (Cmp((yyval.type),"signed long long") == 0) {
		   Delete((yyval.type));
		   (yyval.type) = NewString("long long");
		 }
               }
#line 10031 "y.tab.c" /* yacc.c:1646  */
    break;

  case 388:
#line 5648 "parser.y" /* yacc.c:1646  */
    { 
                 (yyval.ptype) = (yyvsp[0].ptype);
               }
#line 10039 "y.tab.c" /* yacc.c:1646  */
    break;

  case 389:
#line 5651 "parser.y" /* yacc.c:1646  */
    {
                    if ((yyvsp[-1].ptype).us && (yyvsp[0].ptype).us) {
		      Swig_error(cparse_file, cparse_line, "Extra %s specifier.\n", (yyvsp[0].ptype).us);
		    }
                    (yyval.ptype) = (yyvsp[0].ptype);
                    if ((yyvsp[-1].ptype).us) (yyval.ptype).us = (yyvsp[-1].ptype).us;
		    if ((yyvsp[-1].ptype).type) {
		      if (!(yyvsp[0].ptype).type) (yyval.ptype).type = (yyvsp[-1].ptype).type;
		      else {
			int err = 0;
			if ((Cmp((yyvsp[-1].ptype).type,"long") == 0)) {
			  if ((Cmp((yyvsp[0].ptype).type,"long") == 0) || (Strncmp((yyvsp[0].ptype).type,"double",6) == 0)) {
			    (yyval.ptype).type = NewStringf("long %s", (yyvsp[0].ptype).type);
			  } else if (Cmp((yyvsp[0].ptype).type,"int") == 0) {
			    (yyval.ptype).type = (yyvsp[-1].ptype).type;
			  } else {
			    err = 1;
			  }
			} else if ((Cmp((yyvsp[-1].ptype).type,"short")) == 0) {
			  if (Cmp((yyvsp[0].ptype).type,"int") == 0) {
			    (yyval.ptype).type = (yyvsp[-1].ptype).type;
			  } else {
			    err = 1;
			  }
			} else if (Cmp((yyvsp[-1].ptype).type,"int") == 0) {
			  (yyval.ptype).type = (yyvsp[0].ptype).type;
			} else if (Cmp((yyvsp[-1].ptype).type,"double") == 0) {
			  if (Cmp((yyvsp[0].ptype).type,"long") == 0) {
			    (yyval.ptype).type = NewString("long double");
			  } else if (Cmp((yyvsp[0].ptype).type,"complex") == 0) {
			    (yyval.ptype).type = NewString("double complex");
			  } else {
			    err = 1;
			  }
			} else if (Cmp((yyvsp[-1].ptype).type,"float") == 0) {
			  if (Cmp((yyvsp[0].ptype).type,"complex") == 0) {
			    (yyval.ptype).type = NewString("float complex");
			  } else {
			    err = 1;
			  }
			} else if (Cmp((yyvsp[-1].ptype).type,"complex") == 0) {
			  (yyval.ptype).type = NewStringf("%s complex", (yyvsp[0].ptype).type);
			} else {
			  err = 1;
			}
			if (err) {
			  Swig_error(cparse_file, cparse_line, "Extra %s specifier.\n", (yyvsp[-1].ptype).type);
			}
		      }
		    }
               }
#line 10095 "y.tab.c" /* yacc.c:1646  */
    break;

  case 390:
#line 5705 "parser.y" /* yacc.c:1646  */
    { 
		    (yyval.ptype).type = NewString("int");
                    (yyval.ptype).us = 0;
               }
#line 10104 "y.tab.c" /* yacc.c:1646  */
    break;

  case 391:
#line 5709 "parser.y" /* yacc.c:1646  */
    { 
                    (yyval.ptype).type = NewString("short");
                    (yyval.ptype).us = 0;
                }
#line 10113 "y.tab.c" /* yacc.c:1646  */
    break;

  case 392:
#line 5713 "parser.y" /* yacc.c:1646  */
    { 
                    (yyval.ptype).type = NewString("long");
                    (yyval.ptype).us = 0;
                }
#line 10122 "y.tab.c" /* yacc.c:1646  */
    break;

  case 393:
#line 5717 "parser.y" /* yacc.c:1646  */
    { 
                    (yyval.ptype).type = NewString("char");
                    (yyval.ptype).us = 0;
                }
#line 10131 "y.tab.c" /* yacc.c:1646  */
    break;

  case 394:
#line 5721 "parser.y" /* yacc.c:1646  */
    { 
                    (yyval.ptype).type = NewString("wchar_t");
                    (yyval.ptype).us = 0;
                }
#line 10140 "y.tab.c" /* yacc.c:1646  */
    break;

  case 395:
#line 5725 "parser.y" /* yacc.c:1646  */
    { 
                    (yyval.ptype).type = NewString("float");
                    (yyval.ptype).us = 0;
                }
#line 10149 "y.tab.c" /* yacc.c:1646  */
    break;

  case 396:
#line 5729 "parser.y" /* yacc.c:1646  */
    { 
                    (yyval.ptype).type = NewString("double");
                    (yyval.ptype).us = 0;
                }
#line 10158 "y.tab.c" /* yacc.c:1646  */
    break;

  case 397:
#line 5733 "parser.y" /* yacc.c:1646  */
    { 
                    (yyval.ptype).us = NewString("signed");
                    (yyval.ptype).type = 0;
                }
#line 10167 "y.tab.c" /* yacc.c:1646  */
    break;

  case 398:
#line 5737 "parser.y" /* yacc.c:1646  */
    { 
                    (yyval.ptype).us = NewString("unsigned");
                    (yyval.ptype).type = 0;
                }
#line 10176 "y.tab.c" /* yacc.c:1646  */
    break;

  case 399:
#line 5741 "parser.y" /* yacc.c:1646  */
    { 
                    (yyval.ptype).type = NewString("complex");
                    (yyval.ptype).us = 0;
                }
#line 10185 "y.tab.c" /* yacc.c:1646  */
    break;

  case 400:
#line 5745 "parser.y" /* yacc.c:1646  */
    { 
                    (yyval.ptype).type = NewString("__int8");
                    (yyval.ptype).us = 0;
                }
#line 10194 "y.tab.c" /* yacc.c:1646  */
    break;

  case 401:
#line 5749 "parser.y" /* yacc.c:1646  */
    { 
                    (yyval.ptype).type = NewString("__int16");
                    (yyval.ptype).us = 0;
                }
#line 10203 "y.tab.c" /* yacc.c:1646  */
    break;

  case 402:
#line 5753 "parser.y" /* yacc.c:1646  */
    { 
                    (yyval.ptype).type = NewString("__int32");
                    (yyval.ptype).us = 0;
                }
#line 10212 "y.tab.c" /* yacc.c:1646  */
    break;

  case 403:
#line 5757 "parser.y" /* yacc.c:1646  */
    { 
                    (yyval.ptype).type = NewString("__int64");
                    (yyval.ptype).us = 0;
                }
#line 10221 "y.tab.c" /* yacc.c:1646  */
    break;

  case 404:
#line 5763 "parser.y" /* yacc.c:1646  */
    { /* scanner_check_typedef(); */ }
#line 10227 "y.tab.c" /* yacc.c:1646  */
    break;

  case 405:
#line 5763 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.dtype) = (yyvsp[0].dtype);
		   if ((yyval.dtype).type == T_STRING) {
		     (yyval.dtype).rawval = NewStringf("\"%(escape)s\"",(yyval.dtype).val);
		   } else if ((yyval.dtype).type != T_CHAR && (yyval.dtype).type != T_WSTRING && (yyval.dtype).type != T_WCHAR) {
		     (yyval.dtype).rawval = 0;
		   }
		   (yyval.dtype).qualifier = 0;
		   (yyval.dtype).bitfield = 0;
		   (yyval.dtype).throws = 0;
		   (yyval.dtype).throwf = 0;
		   (yyval.dtype).nexcept = 0;
		   scanner_ignore_typedef();
                }
#line 10246 "y.tab.c" /* yacc.c:1646  */
    break;

  case 406:
#line 5777 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.dtype) = (yyvsp[0].dtype);
		}
#line 10254 "y.tab.c" /* yacc.c:1646  */
    break;

  case 407:
#line 5793 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.dtype) = (yyvsp[0].dtype);
		}
#line 10262 "y.tab.c" /* yacc.c:1646  */
    break;

  case 408:
#line 5796 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.dtype) = (yyvsp[0].dtype);
		}
#line 10270 "y.tab.c" /* yacc.c:1646  */
    break;

  case 409:
#line 5802 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.dtype).val = NewString("delete");
		  (yyval.dtype).rawval = 0;
		  (yyval.dtype).type = T_STRING;
		  (yyval.dtype).qualifier = 0;
		  (yyval.dtype).bitfield = 0;
		  (yyval.dtype).throws = 0;
		  (yyval.dtype).throwf = 0;
		  (yyval.dtype).nexcept = 0;
		}
#line 10285 "y.tab.c" /* yacc.c:1646  */
    break;

  case 410:
#line 5815 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.dtype).val = NewString("default");
		  (yyval.dtype).rawval = 0;
		  (yyval.dtype).type = T_STRING;
		  (yyval.dtype).qualifier = 0;
		  (yyval.dtype).bitfield = 0;
		  (yyval.dtype).throws = 0;
		  (yyval.dtype).throwf = 0;
		  (yyval.dtype).nexcept = 0;
		}
#line 10300 "y.tab.c" /* yacc.c:1646  */
    break;

  case 411:
#line 5829 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = (yyvsp[0].id); }
#line 10306 "y.tab.c" /* yacc.c:1646  */
    break;

  case 412:
#line 5830 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = (char *) 0;}
#line 10312 "y.tab.c" /* yacc.c:1646  */
    break;

  case 413:
#line 5833 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = (yyvsp[0].node); }
#line 10318 "y.tab.c" /* yacc.c:1646  */
    break;

  case 414:
#line 5834 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = 0; }
#line 10324 "y.tab.c" /* yacc.c:1646  */
    break;

  case 415:
#line 5838 "parser.y" /* yacc.c:1646  */
    {
		 Node *leftSibling = Getattr((yyvsp[-4].node),"_last");
		 set_nextSibling(leftSibling,(yyvsp[-1].node));
		 Setattr((yyvsp[-4].node),"_last",(yyvsp[-1].node));
		 (yyval.node) = (yyvsp[-4].node);
	       }
#line 10335 "y.tab.c" /* yacc.c:1646  */
    break;

  case 416:
#line 5844 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.node) = (yyvsp[-2].node);
	       }
#line 10343 "y.tab.c" /* yacc.c:1646  */
    break;

  case 417:
#line 5847 "parser.y" /* yacc.c:1646  */
    {
		 Setattr((yyvsp[-1].node),"_last",(yyvsp[-1].node));
		 (yyval.node) = (yyvsp[-1].node);
	       }
#line 10352 "y.tab.c" /* yacc.c:1646  */
    break;

  case 418:
#line 5851 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.node) = 0;
	       }
#line 10360 "y.tab.c" /* yacc.c:1646  */
    break;

  case 419:
#line 5856 "parser.y" /* yacc.c:1646  */
    {
		   SwigType *type = NewSwigType(T_INT);
		   (yyval.node) = new_node("enumitem");
		   Setattr((yyval.node),"name",(yyvsp[0].id));
		   Setattr((yyval.node),"type",type);
		   SetFlag((yyval.node),"feature:immutable");
		   Delete(type);
		 }
#line 10373 "y.tab.c" /* yacc.c:1646  */
    break;

  case 420:
#line 5864 "parser.y" /* yacc.c:1646  */
    {
		   SwigType *type = NewSwigType((yyvsp[0].dtype).type == T_BOOL ? T_BOOL : ((yyvsp[0].dtype).type == T_CHAR ? T_CHAR : T_INT));
		   (yyval.node) = new_node("enumitem");
		   Setattr((yyval.node),"name",(yyvsp[-2].id));
		   Setattr((yyval.node),"type",type);
		   SetFlag((yyval.node),"feature:immutable");
		   Setattr((yyval.node),"enumvalue", (yyvsp[0].dtype).val);
		   Setattr((yyval.node),"value",(yyvsp[-2].id));
		   Delete(type);
                 }
#line 10388 "y.tab.c" /* yacc.c:1646  */
    break;

  case 421:
#line 5876 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.dtype) = (yyvsp[0].dtype);
		   if (((yyval.dtype).type != T_INT) && ((yyval.dtype).type != T_UINT) &&
		       ((yyval.dtype).type != T_LONG) && ((yyval.dtype).type != T_ULONG) &&
		       ((yyval.dtype).type != T_LONGLONG) && ((yyval.dtype).type != T_ULONGLONG) &&
		       ((yyval.dtype).type != T_SHORT) && ((yyval.dtype).type != T_USHORT) &&
		       ((yyval.dtype).type != T_SCHAR) && ((yyval.dtype).type != T_UCHAR) &&
		       ((yyval.dtype).type != T_CHAR) && ((yyval.dtype).type != T_BOOL)) {
		     Swig_error(cparse_file,cparse_line,"Type error. Expecting an integral type\n");
		   }
                }
#line 10404 "y.tab.c" /* yacc.c:1646  */
    break;

  case 422:
#line 5891 "parser.y" /* yacc.c:1646  */
    { (yyval.dtype) = (yyvsp[0].dtype); }
#line 10410 "y.tab.c" /* yacc.c:1646  */
    break;

  case 423:
#line 5892 "parser.y" /* yacc.c:1646  */
    {
		 Node *n;
		 (yyval.dtype).val = (yyvsp[0].type);
		 (yyval.dtype).type = T_INT;
		 /* Check if value is in scope */
		 n = Swig_symbol_clookup((yyvsp[0].type),0);
		 if (n) {
                   /* A band-aid for enum values used in expressions. */
                   if (Strcmp(nodeType(n),"enumitem") == 0) {
                     String *q = Swig_symbol_qualified(n);
                     if (q) {
                       (yyval.dtype).val = NewStringf("%s::%s", q, Getattr(n,"name"));
                       Delete(q);
                     }
                   }
		 }
               }
#line 10432 "y.tab.c" /* yacc.c:1646  */
    break;

  case 424:
#line 5911 "parser.y" /* yacc.c:1646  */
    { (yyval.dtype) = (yyvsp[0].dtype); }
#line 10438 "y.tab.c" /* yacc.c:1646  */
    break;

  case 425:
#line 5912 "parser.y" /* yacc.c:1646  */
    {
		    (yyval.dtype).val = NewString((yyvsp[0].id));
                    (yyval.dtype).type = T_STRING;
               }
#line 10447 "y.tab.c" /* yacc.c:1646  */
    break;

  case 426:
#line 5916 "parser.y" /* yacc.c:1646  */
    {
		  SwigType_push((yyvsp[-2].type),(yyvsp[-1].decl).type);
		  (yyval.dtype).val = NewStringf("sizeof(%s)",SwigType_str((yyvsp[-2].type),0));
		  (yyval.dtype).type = T_ULONG;
               }
#line 10457 "y.tab.c" /* yacc.c:1646  */
    break;

  case 427:
#line 5921 "parser.y" /* yacc.c:1646  */
    {
		  SwigType_push((yyvsp[-2].type),(yyvsp[-1].decl).type);
		  (yyval.dtype).val = NewStringf("sizeof...(%s)",SwigType_str((yyvsp[-2].type),0));
		  (yyval.dtype).type = T_ULONG;
               }
#line 10467 "y.tab.c" /* yacc.c:1646  */
    break;

  case 428:
#line 5926 "parser.y" /* yacc.c:1646  */
    { (yyval.dtype) = (yyvsp[0].dtype); }
#line 10473 "y.tab.c" /* yacc.c:1646  */
    break;

  case 429:
#line 5927 "parser.y" /* yacc.c:1646  */
    {
		    (yyval.dtype).val = NewString((yyvsp[0].id));
		    (yyval.dtype).rawval = NewStringf("L\"%s\"", (yyval.dtype).val);
                    (yyval.dtype).type = T_WSTRING;
	       }
#line 10483 "y.tab.c" /* yacc.c:1646  */
    break;

  case 430:
#line 5932 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.dtype).val = NewString((yyvsp[0].str));
		  if (Len((yyval.dtype).val)) {
		    (yyval.dtype).rawval = NewStringf("'%(escape)s'", (yyval.dtype).val);
		  } else {
		    (yyval.dtype).rawval = NewString("'\\0'");
		  }
		  (yyval.dtype).type = T_CHAR;
		  (yyval.dtype).bitfield = 0;
		  (yyval.dtype).throws = 0;
		  (yyval.dtype).throwf = 0;
		  (yyval.dtype).nexcept = 0;
	       }
#line 10501 "y.tab.c" /* yacc.c:1646  */
    break;

  case 431:
#line 5945 "parser.y" /* yacc.c:1646  */
    {
		  (yyval.dtype).val = NewString((yyvsp[0].str));
		  if (Len((yyval.dtype).val)) {
		    (yyval.dtype).rawval = NewStringf("L\'%s\'", (yyval.dtype).val);
		  } else {
		    (yyval.dtype).rawval = NewString("L'\\0'");
		  }
		  (yyval.dtype).type = T_WCHAR;
		  (yyval.dtype).bitfield = 0;
		  (yyval.dtype).throws = 0;
		  (yyval.dtype).throwf = 0;
		  (yyval.dtype).nexcept = 0;
	       }
#line 10519 "y.tab.c" /* yacc.c:1646  */
    break;

  case 432:
#line 5960 "parser.y" /* yacc.c:1646  */
    {
   	            (yyval.dtype).val = NewStringf("(%s)",(yyvsp[-1].dtype).val);
		    (yyval.dtype).type = (yyvsp[-1].dtype).type;
   	       }
#line 10528 "y.tab.c" /* yacc.c:1646  */
    break;

  case 433:
#line 5967 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.dtype) = (yyvsp[0].dtype);
		 if ((yyvsp[0].dtype).type != T_STRING) {
		   switch ((yyvsp[-2].dtype).type) {
		     case T_FLOAT:
		     case T_DOUBLE:
		     case T_LONGDOUBLE:
		     case T_FLTCPLX:
		     case T_DBLCPLX:
		       (yyval.dtype).val = NewStringf("(%s)%s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val); /* SwigType_str and decimal points don't mix! */
		       break;
		     default:
		       (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[-2].dtype).val,0), (yyvsp[0].dtype).val);
		       break;
		   }
		 }
 	       }
#line 10550 "y.tab.c" /* yacc.c:1646  */
    break;

  case 434:
#line 5984 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.dtype) = (yyvsp[0].dtype);
		 if ((yyvsp[0].dtype).type != T_STRING) {
		   SwigType_push((yyvsp[-3].dtype).val,(yyvsp[-2].type));
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[-3].dtype).val,0), (yyvsp[0].dtype).val);
		 }
 	       }
#line 10562 "y.tab.c" /* yacc.c:1646  */
    break;

  case 435:
#line 5991 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.dtype) = (yyvsp[0].dtype);
		 if ((yyvsp[0].dtype).type != T_STRING) {
		   SwigType_add_reference((yyvsp[-3].dtype).val);
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[-3].dtype).val,0), (yyvsp[0].dtype).val);
		 }
 	       }
#line 10574 "y.tab.c" /* yacc.c:1646  */
    break;

  case 436:
#line 5998 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.dtype) = (yyvsp[0].dtype);
		 if ((yyvsp[0].dtype).type != T_STRING) {
		   SwigType_add_rvalue_reference((yyvsp[-3].dtype).val);
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[-3].dtype).val,0), (yyvsp[0].dtype).val);
		 }
 	       }
#line 10586 "y.tab.c" /* yacc.c:1646  */
    break;

  case 437:
#line 6005 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.dtype) = (yyvsp[0].dtype);
		 if ((yyvsp[0].dtype).type != T_STRING) {
		   SwigType_push((yyvsp[-4].dtype).val,(yyvsp[-3].type));
		   SwigType_add_reference((yyvsp[-4].dtype).val);
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[-4].dtype).val,0), (yyvsp[0].dtype).val);
		 }
 	       }
#line 10599 "y.tab.c" /* yacc.c:1646  */
    break;

  case 438:
#line 6013 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.dtype) = (yyvsp[0].dtype);
		 if ((yyvsp[0].dtype).type != T_STRING) {
		   SwigType_push((yyvsp[-4].dtype).val,(yyvsp[-3].type));
		   SwigType_add_rvalue_reference((yyvsp[-4].dtype).val);
		   (yyval.dtype).val = NewStringf("(%s) %s", SwigType_str((yyvsp[-4].dtype).val,0), (yyvsp[0].dtype).val);
		 }
 	       }
#line 10612 "y.tab.c" /* yacc.c:1646  */
    break;

  case 439:
#line 6021 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype) = (yyvsp[0].dtype);
                 (yyval.dtype).val = NewStringf("&%s",(yyvsp[0].dtype).val);
	       }
#line 10621 "y.tab.c" /* yacc.c:1646  */
    break;

  case 440:
#line 6025 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype) = (yyvsp[0].dtype);
                 (yyval.dtype).val = NewStringf("&&%s",(yyvsp[0].dtype).val);
	       }
#line 10630 "y.tab.c" /* yacc.c:1646  */
    break;

  case 441:
#line 6029 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype) = (yyvsp[0].dtype);
                 (yyval.dtype).val = NewStringf("*%s",(yyvsp[0].dtype).val);
	       }
#line 10639 "y.tab.c" /* yacc.c:1646  */
    break;

  case 442:
#line 6035 "parser.y" /* yacc.c:1646  */
    { (yyval.dtype) = (yyvsp[0].dtype); }
#line 10645 "y.tab.c" /* yacc.c:1646  */
    break;

  case 443:
#line 6036 "parser.y" /* yacc.c:1646  */
    { (yyval.dtype) = (yyvsp[0].dtype); }
#line 10651 "y.tab.c" /* yacc.c:1646  */
    break;

  case 444:
#line 6037 "parser.y" /* yacc.c:1646  */
    { (yyval.dtype) = (yyvsp[0].dtype); }
#line 10657 "y.tab.c" /* yacc.c:1646  */
    break;

  case 445:
#line 6038 "parser.y" /* yacc.c:1646  */
    { (yyval.dtype) = (yyvsp[0].dtype); }
#line 10663 "y.tab.c" /* yacc.c:1646  */
    break;

  case 446:
#line 6039 "parser.y" /* yacc.c:1646  */
    { (yyval.dtype) = (yyvsp[0].dtype); }
#line 10669 "y.tab.c" /* yacc.c:1646  */
    break;

  case 447:
#line 6040 "parser.y" /* yacc.c:1646  */
    { (yyval.dtype) = (yyvsp[0].dtype); }
#line 10675 "y.tab.c" /* yacc.c:1646  */
    break;

  case 448:
#line 6041 "parser.y" /* yacc.c:1646  */
    { (yyval.dtype) = (yyvsp[0].dtype); }
#line 10681 "y.tab.c" /* yacc.c:1646  */
    break;

  case 449:
#line 6042 "parser.y" /* yacc.c:1646  */
    { (yyval.dtype) = (yyvsp[0].dtype); }
#line 10687 "y.tab.c" /* yacc.c:1646  */
    break;

  case 450:
#line 6045 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype).val = NewStringf("%s+%s",(yyvsp[-2].dtype).val,(yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 10696 "y.tab.c" /* yacc.c:1646  */
    break;

  case 451:
#line 6049 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype).val = NewStringf("%s-%s",(yyvsp[-2].dtype).val,(yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 10705 "y.tab.c" /* yacc.c:1646  */
    break;

  case 452:
#line 6053 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype).val = NewStringf("%s*%s",(yyvsp[-2].dtype).val,(yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 10714 "y.tab.c" /* yacc.c:1646  */
    break;

  case 453:
#line 6057 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype).val = NewStringf("%s/%s",(yyvsp[-2].dtype).val,(yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 10723 "y.tab.c" /* yacc.c:1646  */
    break;

  case 454:
#line 6061 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype).val = NewStringf("%s%%%s",(yyvsp[-2].dtype).val,(yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 10732 "y.tab.c" /* yacc.c:1646  */
    break;

  case 455:
#line 6065 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype).val = NewStringf("%s&%s",(yyvsp[-2].dtype).val,(yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 10741 "y.tab.c" /* yacc.c:1646  */
    break;

  case 456:
#line 6069 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype).val = NewStringf("%s|%s",(yyvsp[-2].dtype).val,(yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 10750 "y.tab.c" /* yacc.c:1646  */
    break;

  case 457:
#line 6073 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype).val = NewStringf("%s^%s",(yyvsp[-2].dtype).val,(yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type,(yyvsp[0].dtype).type);
	       }
#line 10759 "y.tab.c" /* yacc.c:1646  */
    break;

  case 458:
#line 6077 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype).val = NewStringf("%s << %s",(yyvsp[-2].dtype).val,(yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote_type((yyvsp[-2].dtype).type);
	       }
#line 10768 "y.tab.c" /* yacc.c:1646  */
    break;

  case 459:
#line 6081 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype).val = NewStringf("%s >> %s",(yyvsp[-2].dtype).val,(yyvsp[0].dtype).val);
		 (yyval.dtype).type = promote_type((yyvsp[-2].dtype).type);
	       }
#line 10777 "y.tab.c" /* yacc.c:1646  */
    break;

  case 460:
#line 6085 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype).val = NewStringf("%s&&%s",(yyvsp[-2].dtype).val,(yyvsp[0].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 10786 "y.tab.c" /* yacc.c:1646  */
    break;

  case 461:
#line 6089 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype).val = NewStringf("%s||%s",(yyvsp[-2].dtype).val,(yyvsp[0].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 10795 "y.tab.c" /* yacc.c:1646  */
    break;

  case 462:
#line 6093 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype).val = NewStringf("%s==%s",(yyvsp[-2].dtype).val,(yyvsp[0].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 10804 "y.tab.c" /* yacc.c:1646  */
    break;

  case 463:
#line 6097 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype).val = NewStringf("%s!=%s",(yyvsp[-2].dtype).val,(yyvsp[0].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 10813 "y.tab.c" /* yacc.c:1646  */
    break;

  case 464:
#line 6111 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype).val = NewStringf("%s >= %s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 10822 "y.tab.c" /* yacc.c:1646  */
    break;

  case 465:
#line 6115 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype).val = NewStringf("%s <= %s", (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 (yyval.dtype).type = cparse_cplusplus ? T_BOOL : T_INT;
	       }
#line 10831 "y.tab.c" /* yacc.c:1646  */
    break;

  case 466:
#line 6119 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype).val = NewStringf("%s?%s:%s", (yyvsp[-4].dtype).val, (yyvsp[-2].dtype).val, (yyvsp[0].dtype).val);
		 /* This may not be exactly right, but is probably good enough
		  * for the purposes of parsing constant expressions. */
		 (yyval.dtype).type = promote((yyvsp[-2].dtype).type, (yyvsp[0].dtype).type);
	       }
#line 10842 "y.tab.c" /* yacc.c:1646  */
    break;

  case 467:
#line 6125 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype).val = NewStringf("-%s",(yyvsp[0].dtype).val);
		 (yyval.dtype).type = (yyvsp[0].dtype).type;
	       }
#line 10851 "y.tab.c" /* yacc.c:1646  */
    break;

  case 468:
#line 6129 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.dtype).val = NewStringf("+%s",(yyvsp[0].dtype).val);
		 (yyval.dtype).type = (yyvsp[0].dtype).type;
	       }
#line 10860 "y.tab.c" /* yacc.c:1646  */
    break;

  case 469:
#line 6133 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.dtype).val = NewStringf("~%s",(yyvsp[0].dtype).val);
		 (yyval.dtype).type = (yyvsp[0].dtype).type;
	       }
#line 10869 "y.tab.c" /* yacc.c:1646  */
    break;

  case 470:
#line 6137 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.dtype).val = NewStringf("!%s",(yyvsp[0].dtype).val);
		 (yyval.dtype).type = T_INT;
	       }
#line 10878 "y.tab.c" /* yacc.c:1646  */
    break;

  case 471:
#line 6141 "parser.y" /* yacc.c:1646  */
    {
		 String *qty;
                 skip_balanced('(',')');
		 qty = Swig_symbol_type_qualify((yyvsp[-1].type),0);
		 if (SwigType_istemplate(qty)) {
		   String *nstr = SwigType_namestr(qty);
		   Delete(qty);
		   qty = nstr;
		 }
		 (yyval.dtype).val = NewStringf("%s%s",qty,scanner_ccode);
		 Clear(scanner_ccode);
		 (yyval.dtype).type = T_INT;
		 Delete(qty);
               }
#line 10897 "y.tab.c" /* yacc.c:1646  */
    break;

  case 472:
#line 6157 "parser.y" /* yacc.c:1646  */
    {
	        (yyval.str) = NewString("...");
	      }
#line 10905 "y.tab.c" /* yacc.c:1646  */
    break;

  case 473:
#line 6162 "parser.y" /* yacc.c:1646  */
    {
	        (yyval.str) = (yyvsp[0].str);
	      }
#line 10913 "y.tab.c" /* yacc.c:1646  */
    break;

  case 474:
#line 6165 "parser.y" /* yacc.c:1646  */
    {
	        (yyval.str) = 0;
	      }
#line 10921 "y.tab.c" /* yacc.c:1646  */
    break;

  case 475:
#line 6170 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.bases) = (yyvsp[0].bases);
               }
#line 10929 "y.tab.c" /* yacc.c:1646  */
    break;

  case 476:
#line 6175 "parser.y" /* yacc.c:1646  */
    { inherit_list = 1; }
#line 10935 "y.tab.c" /* yacc.c:1646  */
    break;

  case 477:
#line 6175 "parser.y" /* yacc.c:1646  */
    { (yyval.bases) = (yyvsp[0].bases); inherit_list = 0; }
#line 10941 "y.tab.c" /* yacc.c:1646  */
    break;

  case 478:
#line 6176 "parser.y" /* yacc.c:1646  */
    { (yyval.bases) = 0; }
#line 10947 "y.tab.c" /* yacc.c:1646  */
    break;

  case 479:
#line 6179 "parser.y" /* yacc.c:1646  */
    {
		   Hash *list = NewHash();
		   Node *base = (yyvsp[0].node);
		   Node *name = Getattr(base,"name");
		   List *lpublic = NewList();
		   List *lprotected = NewList();
		   List *lprivate = NewList();
		   Setattr(list,"public",lpublic);
		   Setattr(list,"protected",lprotected);
		   Setattr(list,"private",lprivate);
		   Delete(lpublic);
		   Delete(lprotected);
		   Delete(lprivate);
		   Append(Getattr(list,Getattr(base,"access")),name);
	           (yyval.bases) = list;
               }
#line 10968 "y.tab.c" /* yacc.c:1646  */
    break;

  case 480:
#line 6196 "parser.y" /* yacc.c:1646  */
    {
		   Hash *list = (yyvsp[-2].bases);
		   Node *base = (yyvsp[0].node);
		   Node *name = Getattr(base,"name");
		   Append(Getattr(list,Getattr(base,"access")),name);
                   (yyval.bases) = list;
               }
#line 10980 "y.tab.c" /* yacc.c:1646  */
    break;

  case 481:
#line 6205 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.intvalue) = cparse_line;
	       }
#line 10988 "y.tab.c" /* yacc.c:1646  */
    break;

  case 482:
#line 6207 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.node) = NewHash();
		 Setfile((yyval.node),cparse_file);
		 Setline((yyval.node),(yyvsp[-2].intvalue));
		 Setattr((yyval.node),"name",(yyvsp[-1].str));
		 Setfile((yyvsp[-1].str),cparse_file);
		 Setline((yyvsp[-1].str),(yyvsp[-2].intvalue));
                 if (last_cpptype && (Strcmp(last_cpptype,"struct") != 0)) {
		   Setattr((yyval.node),"access","private");
		   Swig_warning(WARN_PARSE_NO_ACCESS, Getfile((yyval.node)), Getline((yyval.node)), "No access specifier given for base class '%s' (ignored).\n", SwigType_namestr((yyvsp[-1].str)));
                 } else {
		   Setattr((yyval.node),"access","public");
		 }
		 if ((yyvsp[0].str))
		   SetFlag((yyval.node), "variadic");
               }
#line 11009 "y.tab.c" /* yacc.c:1646  */
    break;

  case 483:
#line 6223 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.intvalue) = cparse_line;
	       }
#line 11017 "y.tab.c" /* yacc.c:1646  */
    break;

  case 484:
#line 6225 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.node) = NewHash();
		 Setfile((yyval.node),cparse_file);
		 Setline((yyval.node),(yyvsp[-3].intvalue));
		 Setattr((yyval.node),"name",(yyvsp[-1].str));
		 Setfile((yyvsp[-1].str),cparse_file);
		 Setline((yyvsp[-1].str),(yyvsp[-3].intvalue));
		 Setattr((yyval.node),"access",(yyvsp[-4].id));
	         if (Strcmp((yyvsp[-4].id),"public") != 0) {
		   Swig_warning(WARN_PARSE_PRIVATE_INHERIT, Getfile((yyval.node)), Getline((yyval.node)), "%s inheritance from base '%s' (ignored).\n", (yyvsp[-4].id), SwigType_namestr((yyvsp[-1].str)));
		 }
		 if ((yyvsp[0].str))
		   SetFlag((yyval.node), "variadic");
               }
#line 11036 "y.tab.c" /* yacc.c:1646  */
    break;

  case 485:
#line 6241 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = (char*)"public"; }
#line 11042 "y.tab.c" /* yacc.c:1646  */
    break;

  case 486:
#line 6242 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = (char*)"private"; }
#line 11048 "y.tab.c" /* yacc.c:1646  */
    break;

  case 487:
#line 6243 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = (char*)"protected"; }
#line 11054 "y.tab.c" /* yacc.c:1646  */
    break;

  case 488:
#line 6247 "parser.y" /* yacc.c:1646  */
    { 
                   (yyval.id) = (char*)"class"; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
#line 11063 "y.tab.c" /* yacc.c:1646  */
    break;

  case 489:
#line 6251 "parser.y" /* yacc.c:1646  */
    { 
                   (yyval.id) = (char *)"typename"; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
#line 11072 "y.tab.c" /* yacc.c:1646  */
    break;

  case 490:
#line 6255 "parser.y" /* yacc.c:1646  */
    { 
                   (yyval.id) = (char *)"class..."; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
#line 11081 "y.tab.c" /* yacc.c:1646  */
    break;

  case 491:
#line 6259 "parser.y" /* yacc.c:1646  */
    { 
                   (yyval.id) = (char *)"typename..."; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
#line 11090 "y.tab.c" /* yacc.c:1646  */
    break;

  case 492:
#line 6265 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.id) = (yyvsp[0].id);
               }
#line 11098 "y.tab.c" /* yacc.c:1646  */
    break;

  case 493:
#line 6268 "parser.y" /* yacc.c:1646  */
    { 
                   (yyval.id) = (char*)"struct"; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
#line 11107 "y.tab.c" /* yacc.c:1646  */
    break;

  case 494:
#line 6272 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.id) = (char*)"union"; 
		   if (!inherit_list) last_cpptype = (yyval.id);
               }
#line 11116 "y.tab.c" /* yacc.c:1646  */
    break;

  case 497:
#line 6282 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.str) = 0;
	       }
#line 11124 "y.tab.c" /* yacc.c:1646  */
    break;

  case 498:
#line 6285 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.str) = 0;
	       }
#line 11132 "y.tab.c" /* yacc.c:1646  */
    break;

  case 499:
#line 6288 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.str) = 0;
	       }
#line 11140 "y.tab.c" /* yacc.c:1646  */
    break;

  case 500:
#line 6291 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.str) = 0;
	       }
#line 11148 "y.tab.c" /* yacc.c:1646  */
    break;

  case 501:
#line 6296 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.dtype).throws = (yyvsp[-1].pl);
                    (yyval.dtype).throwf = NewString("1");
                    (yyval.dtype).nexcept = 0;
	       }
#line 11158 "y.tab.c" /* yacc.c:1646  */
    break;

  case 502:
#line 6301 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.dtype).throws = 0;
                    (yyval.dtype).throwf = 0;
                    (yyval.dtype).nexcept = NewString("true");
	       }
#line 11168 "y.tab.c" /* yacc.c:1646  */
    break;

  case 503:
#line 6306 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.dtype).throws = 0;
                    (yyval.dtype).throwf = 0;
                    (yyval.dtype).nexcept = 0;
	       }
#line 11178 "y.tab.c" /* yacc.c:1646  */
    break;

  case 504:
#line 6311 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.dtype).throws = 0;
                    (yyval.dtype).throwf = 0;
                    (yyval.dtype).nexcept = NewString("true");
	       }
#line 11188 "y.tab.c" /* yacc.c:1646  */
    break;

  case 505:
#line 6316 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.dtype).throws = 0;
                    (yyval.dtype).throwf = 0;
                    (yyval.dtype).nexcept = (yyvsp[-1].dtype).val;
	       }
#line 11198 "y.tab.c" /* yacc.c:1646  */
    break;

  case 506:
#line 6323 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.dtype).throws = 0;
                    (yyval.dtype).throwf = 0;
                    (yyval.dtype).nexcept = 0;
                    (yyval.dtype).qualifier = (yyvsp[0].str);
               }
#line 11209 "y.tab.c" /* yacc.c:1646  */
    break;

  case 507:
#line 6329 "parser.y" /* yacc.c:1646  */
    {
		    (yyval.dtype) = (yyvsp[0].dtype);
                    (yyval.dtype).qualifier = 0;
               }
#line 11218 "y.tab.c" /* yacc.c:1646  */
    break;

  case 508:
#line 6333 "parser.y" /* yacc.c:1646  */
    {
		    (yyval.dtype) = (yyvsp[0].dtype);
                    (yyval.dtype).qualifier = (yyvsp[-1].str);
               }
#line 11227 "y.tab.c" /* yacc.c:1646  */
    break;

  case 509:
#line 6337 "parser.y" /* yacc.c:1646  */
    { 
                    (yyval.dtype).throws = 0;
                    (yyval.dtype).throwf = 0;
                    (yyval.dtype).nexcept = 0;
                    (yyval.dtype).qualifier = 0; 
               }
#line 11238 "y.tab.c" /* yacc.c:1646  */
    break;

  case 510:
#line 6345 "parser.y" /* yacc.c:1646  */
    { 
                    Clear(scanner_ccode); 
                    (yyval.decl).have_parms = 0; 
                    (yyval.decl).defarg = 0; 
		    (yyval.decl).throws = (yyvsp[-2].dtype).throws;
		    (yyval.decl).throwf = (yyvsp[-2].dtype).throwf;
		    (yyval.decl).nexcept = (yyvsp[-2].dtype).nexcept;
               }
#line 11251 "y.tab.c" /* yacc.c:1646  */
    break;

  case 511:
#line 6353 "parser.y" /* yacc.c:1646  */
    { 
                    skip_balanced('{','}'); 
                    (yyval.decl).have_parms = 0; 
                    (yyval.decl).defarg = 0; 
                    (yyval.decl).throws = (yyvsp[-2].dtype).throws;
                    (yyval.decl).throwf = (yyvsp[-2].dtype).throwf;
                    (yyval.decl).nexcept = (yyvsp[-2].dtype).nexcept;
               }
#line 11264 "y.tab.c" /* yacc.c:1646  */
    break;

  case 512:
#line 6361 "parser.y" /* yacc.c:1646  */
    { 
                    Clear(scanner_ccode); 
                    (yyval.decl).parms = (yyvsp[-2].pl); 
                    (yyval.decl).have_parms = 1; 
                    (yyval.decl).defarg = 0; 
		    (yyval.decl).throws = 0;
		    (yyval.decl).throwf = 0;
		    (yyval.decl).nexcept = 0;
               }
#line 11278 "y.tab.c" /* yacc.c:1646  */
    break;

  case 513:
#line 6370 "parser.y" /* yacc.c:1646  */
    {
                    skip_balanced('{','}'); 
                    (yyval.decl).parms = (yyvsp[-2].pl); 
                    (yyval.decl).have_parms = 1; 
                    (yyval.decl).defarg = 0; 
                    (yyval.decl).throws = 0;
                    (yyval.decl).throwf = 0;
                    (yyval.decl).nexcept = 0;
               }
#line 11292 "y.tab.c" /* yacc.c:1646  */
    break;

  case 514:
#line 6379 "parser.y" /* yacc.c:1646  */
    { 
                    (yyval.decl).have_parms = 0; 
                    (yyval.decl).defarg = (yyvsp[-1].dtype).val; 
                    (yyval.decl).throws = 0;
                    (yyval.decl).throwf = 0;
                    (yyval.decl).nexcept = 0;
               }
#line 11304 "y.tab.c" /* yacc.c:1646  */
    break;

  case 515:
#line 6386 "parser.y" /* yacc.c:1646  */
    {
                    (yyval.decl).have_parms = 0;
                    (yyval.decl).defarg = (yyvsp[-1].dtype).val;
                    (yyval.decl).throws = (yyvsp[-3].dtype).throws;
                    (yyval.decl).throwf = (yyvsp[-3].dtype).throwf;
                    (yyval.decl).nexcept = (yyvsp[-3].dtype).nexcept;
               }
#line 11316 "y.tab.c" /* yacc.c:1646  */
    break;

  case 522:
#line 6405 "parser.y" /* yacc.c:1646  */
    {
		  skip_balanced('(',')');
		  Clear(scanner_ccode);
		}
#line 11325 "y.tab.c" /* yacc.c:1646  */
    break;

  case 523:
#line 6417 "parser.y" /* yacc.c:1646  */
    {
		  skip_balanced('{','}');
		  Clear(scanner_ccode);
		}
#line 11334 "y.tab.c" /* yacc.c:1646  */
    break;

  case 524:
#line 6423 "parser.y" /* yacc.c:1646  */
    { 
                     String *s = NewStringEmpty();
                     SwigType_add_template(s,(yyvsp[-1].p));
                     (yyval.id) = Char(s);
		     scanner_last_id(1);
                 }
#line 11345 "y.tab.c" /* yacc.c:1646  */
    break;

  case 525:
#line 6429 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = (char*)"";  }
#line 11351 "y.tab.c" /* yacc.c:1646  */
    break;

  case 526:
#line 6433 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = (yyvsp[0].id); }
#line 11357 "y.tab.c" /* yacc.c:1646  */
    break;

  case 527:
#line 6434 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = Swig_copy_string("override"); }
#line 11363 "y.tab.c" /* yacc.c:1646  */
    break;

  case 528:
#line 6435 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = Swig_copy_string("final"); }
#line 11369 "y.tab.c" /* yacc.c:1646  */
    break;

  case 529:
#line 6438 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = (yyvsp[0].id); }
#line 11375 "y.tab.c" /* yacc.c:1646  */
    break;

  case 530:
#line 6439 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = (yyvsp[0].dtype).val; }
#line 11381 "y.tab.c" /* yacc.c:1646  */
    break;

  case 531:
#line 6440 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = (yyvsp[0].id); }
#line 11387 "y.tab.c" /* yacc.c:1646  */
    break;

  case 532:
#line 6443 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = (yyvsp[0].id); }
#line 11393 "y.tab.c" /* yacc.c:1646  */
    break;

  case 533:
#line 6444 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = 0; }
#line 11399 "y.tab.c" /* yacc.c:1646  */
    break;

  case 534:
#line 6447 "parser.y" /* yacc.c:1646  */
    { 
                  (yyval.str) = 0;
		  if (!(yyval.str)) (yyval.str) = NewStringf("%s%s", (yyvsp[-1].str),(yyvsp[0].str));
      	          Delete((yyvsp[0].str));
               }
#line 11409 "y.tab.c" /* yacc.c:1646  */
    break;

  case 535:
#line 6452 "parser.y" /* yacc.c:1646  */
    { 
		 (yyval.str) = NewStringf("::%s%s",(yyvsp[-1].str),(yyvsp[0].str));
                 Delete((yyvsp[0].str));
               }
#line 11418 "y.tab.c" /* yacc.c:1646  */
    break;

  case 536:
#line 6456 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.str) = NewString((yyvsp[0].str));
   	       }
#line 11426 "y.tab.c" /* yacc.c:1646  */
    break;

  case 537:
#line 6459 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.str) = NewStringf("::%s",(yyvsp[0].str));
               }
#line 11434 "y.tab.c" /* yacc.c:1646  */
    break;

  case 538:
#line 6462 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.str) = NewString((yyvsp[0].str));
	       }
#line 11442 "y.tab.c" /* yacc.c:1646  */
    break;

  case 539:
#line 6465 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.str) = NewStringf("::%s",(yyvsp[0].str));
               }
#line 11450 "y.tab.c" /* yacc.c:1646  */
    break;

  case 540:
#line 6470 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.str) = NewStringf("::%s%s",(yyvsp[-1].str),(yyvsp[0].str));
		   Delete((yyvsp[0].str));
               }
#line 11459 "y.tab.c" /* yacc.c:1646  */
    break;

  case 541:
#line 6474 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.str) = NewStringf("::%s",(yyvsp[0].str));
               }
#line 11467 "y.tab.c" /* yacc.c:1646  */
    break;

  case 542:
#line 6477 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.str) = NewStringf("::%s",(yyvsp[0].str));
               }
#line 11475 "y.tab.c" /* yacc.c:1646  */
    break;

  case 543:
#line 6484 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.str) = NewStringf("::~%s",(yyvsp[0].str));
               }
#line 11483 "y.tab.c" /* yacc.c:1646  */
    break;

  case 544:
#line 6490 "parser.y" /* yacc.c:1646  */
    {
                  (yyval.str) = NewStringf("%s%s",(yyvsp[-1].id),(yyvsp[0].id));
		  /*		  if (Len($2)) {
		    scanner_last_id(1);
		    } */
              }
#line 11494 "y.tab.c" /* yacc.c:1646  */
    break;

  case 545:
#line 6499 "parser.y" /* yacc.c:1646  */
    {
                  (yyval.str) = 0;
		  if (!(yyval.str)) (yyval.str) = NewStringf("%s%s", (yyvsp[-1].id),(yyvsp[0].str));
      	          Delete((yyvsp[0].str));
               }
#line 11504 "y.tab.c" /* yacc.c:1646  */
    break;

  case 546:
#line 6504 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.str) = NewStringf("::%s%s",(yyvsp[-1].id),(yyvsp[0].str));
                 Delete((yyvsp[0].str));
               }
#line 11513 "y.tab.c" /* yacc.c:1646  */
    break;

  case 547:
#line 6508 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.str) = NewString((yyvsp[0].id));
   	       }
#line 11521 "y.tab.c" /* yacc.c:1646  */
    break;

  case 548:
#line 6511 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.str) = NewStringf("::%s",(yyvsp[0].id));
               }
#line 11529 "y.tab.c" /* yacc.c:1646  */
    break;

  case 549:
#line 6514 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.str) = NewString((yyvsp[0].str));
	       }
#line 11537 "y.tab.c" /* yacc.c:1646  */
    break;

  case 550:
#line 6517 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.str) = NewStringf("::%s",(yyvsp[0].str));
               }
#line 11545 "y.tab.c" /* yacc.c:1646  */
    break;

  case 551:
#line 6522 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.str) = NewStringf("::%s%s",(yyvsp[-1].id),(yyvsp[0].str));
		   Delete((yyvsp[0].str));
               }
#line 11554 "y.tab.c" /* yacc.c:1646  */
    break;

  case 552:
#line 6526 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.str) = NewStringf("::%s",(yyvsp[0].id));
               }
#line 11562 "y.tab.c" /* yacc.c:1646  */
    break;

  case 553:
#line 6529 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.str) = NewStringf("::%s",(yyvsp[0].str));
               }
#line 11570 "y.tab.c" /* yacc.c:1646  */
    break;

  case 554:
#line 6532 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.str) = NewStringf("::~%s",(yyvsp[0].id));
               }
#line 11578 "y.tab.c" /* yacc.c:1646  */
    break;

  case 555:
#line 6538 "parser.y" /* yacc.c:1646  */
    { 
                   (yyval.id) = (char *) malloc(strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+1);
                   strcpy((yyval.id),(yyvsp[-1].id));
                   strcat((yyval.id),(yyvsp[0].id));
               }
#line 11588 "y.tab.c" /* yacc.c:1646  */
    break;

  case 556:
#line 6543 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = (yyvsp[0].id);}
#line 11594 "y.tab.c" /* yacc.c:1646  */
    break;

  case 557:
#line 6546 "parser.y" /* yacc.c:1646  */
    {
                   (yyval.id) = (char *) malloc(strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+1);
                   strcpy((yyval.id),(yyvsp[-1].id));
                   strcat((yyval.id),(yyvsp[0].id));
               }
#line 11604 "y.tab.c" /* yacc.c:1646  */
    break;

  case 558:
#line 6558 "parser.y" /* yacc.c:1646  */
    { (yyval.id) = (yyvsp[0].id);}
#line 11610 "y.tab.c" /* yacc.c:1646  */
    break;

  case 559:
#line 6561 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.str) = NewString((yyvsp[0].id));
               }
#line 11618 "y.tab.c" /* yacc.c:1646  */
    break;

  case 560:
#line 6564 "parser.y" /* yacc.c:1646  */
    {
                  skip_balanced('{','}');
		  (yyval.str) = NewString(scanner_ccode);
               }
#line 11627 "y.tab.c" /* yacc.c:1646  */
    break;

  case 561:
#line 6568 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.str) = (yyvsp[0].str);
              }
#line 11635 "y.tab.c" /* yacc.c:1646  */
    break;

  case 562:
#line 6573 "parser.y" /* yacc.c:1646  */
    {
                  Hash *n;
                  (yyval.node) = NewHash();
                  n = (yyvsp[-1].node);
                  while(n) {
                     String *name, *value;
                     name = Getattr(n,"name");
                     value = Getattr(n,"value");
		     if (!value) value = (String *) "1";
                     Setattr((yyval.node),name, value);
		     n = nextSibling(n);
		  }
               }
#line 11653 "y.tab.c" /* yacc.c:1646  */
    break;

  case 563:
#line 6586 "parser.y" /* yacc.c:1646  */
    { (yyval.node) = 0; }
#line 11659 "y.tab.c" /* yacc.c:1646  */
    break;

  case 564:
#line 6590 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.node) = NewHash();
		 Setattr((yyval.node),"name",(yyvsp[-2].id));
		 Setattr((yyval.node),"value",(yyvsp[0].id));
               }
#line 11669 "y.tab.c" /* yacc.c:1646  */
    break;

  case 565:
#line 6595 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.node) = NewHash();
		 Setattr((yyval.node),"name",(yyvsp[-4].id));
		 Setattr((yyval.node),"value",(yyvsp[-2].id));
		 set_nextSibling((yyval.node),(yyvsp[0].node));
               }
#line 11680 "y.tab.c" /* yacc.c:1646  */
    break;

  case 566:
#line 6601 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.node) = NewHash();
                 Setattr((yyval.node),"name",(yyvsp[0].id));
	       }
#line 11689 "y.tab.c" /* yacc.c:1646  */
    break;

  case 567:
#line 6605 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.node) = NewHash();
                 Setattr((yyval.node),"name",(yyvsp[-2].id));
                 set_nextSibling((yyval.node),(yyvsp[0].node));
               }
#line 11699 "y.tab.c" /* yacc.c:1646  */
    break;

  case 568:
#line 6610 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.node) = (yyvsp[0].node);
		 Setattr((yyval.node),"name",(yyvsp[-2].id));
               }
#line 11708 "y.tab.c" /* yacc.c:1646  */
    break;

  case 569:
#line 6614 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.node) = (yyvsp[-2].node);
		 Setattr((yyval.node),"name",(yyvsp[-4].id));
		 set_nextSibling((yyval.node),(yyvsp[0].node));
               }
#line 11718 "y.tab.c" /* yacc.c:1646  */
    break;

  case 570:
#line 6621 "parser.y" /* yacc.c:1646  */
    {
		 (yyval.id) = (yyvsp[0].id);
               }
#line 11726 "y.tab.c" /* yacc.c:1646  */
    break;

  case 571:
#line 6624 "parser.y" /* yacc.c:1646  */
    {
                 (yyval.id) = Char((yyvsp[0].dtype).val);
               }
#line 11734 "y.tab.c" /* yacc.c:1646  */
    break;


#line 11738 "y.tab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 6631 "parser.y" /* yacc.c:1906  */


SwigType *Swig_cparse_type(String *s) {
   String *ns;
   ns = NewStringf("%s;",s);
   Seek(ns,0,SEEK_SET);
   scanner_file(ns);
   top = 0;
   scanner_next_token(PARSETYPE);
   yyparse();
   /*   Printf(stdout,"typeparse: '%s' ---> '%s'\n", s, top); */
   return top;
}


Parm *Swig_cparse_parm(String *s) {
   String *ns;
   ns = NewStringf("%s;",s);
   Seek(ns,0,SEEK_SET);
   scanner_file(ns);
   top = 0;
   scanner_next_token(PARSEPARM);
   yyparse();
   /*   Printf(stdout,"typeparse: '%s' ---> '%s'\n", s, top); */
   Delete(ns);
   return top;
}


ParmList *Swig_cparse_parms(String *s, Node *file_line_node) {
   String *ns;
   char *cs = Char(s);
   if (cs && cs[0] != '(') {
     ns = NewStringf("(%s);",s);
   } else {
     ns = NewStringf("%s;",s);
   }
   Setfile(ns, Getfile(file_line_node));
   Setline(ns, Getline(file_line_node));
   Seek(ns,0,SEEK_SET);
   scanner_file(ns);
   top = 0;
   scanner_next_token(PARSEPARMS);
   yyparse();
   /*   Printf(stdout,"typeparse: '%s' ---> '%s'\n", s, top); */
   return top;
}

