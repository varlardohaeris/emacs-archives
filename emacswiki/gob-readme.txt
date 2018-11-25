This library means to help create gobject quickly.
It provide a widget user interface to configure object
as well as abbrev interface to quick insert code.
To invoke widget user interface, use M-x gob-new-object.
To use it as abbrev, you need some elisp to configure tempo.
This an example of configuration:
  (require 'tempo)
  (defvar tempo-c-tags nil)
  (setq tempo-interactive t)
  (defun tempo-space ()
    "if has match something in tempo-tags, expand, otherwise insert
  space. "
    (interactive)
    (if (tempo-expand-if-complete)
        nil
      (call-interactively 'self-insert-command)))
  (defun tempo-install (&optional finder tags)
    "install tempo for certain major mode"
    (if finder (setq tempo-match-finder finder))
    (if tags (tempo-use-tag-list tags))
    (local-set-key " " 'tempo-space))
   (require 'gob)
   (add-hook 'c-mode-hook
             (lambda ()
               (tempo-install nil 'tempo-c-tags)
               (gob-install-tempo)))
Now, in c-mode, you can use abbrev "headerx", "sourcex", "macrox",
and so on to quick insert code snips.
Another way for insert template, is using command M-x gob-header
and M-x gob-source.
Note that the only way to create code for install object data
member, is using the widget user interface.

Quick start:
To create a simple object, do as following:
1. M-x gob-new-object RET
2. fill the form, typically you should set class, parent class,
   and data members. For example, set as following:
    class        => demo_person
    parent class => g_object
    members      => name, gchar *
                    age, gint
3. press button "Generate Code", and save the header and source file
4. create a test file demoperson-t.c:
     /* demo-person-t.c --- test file for class person */
     #include "demoperson.h"
     #include <stdio.h>
     int main (int argc, char * argv[])
     {
         DemoPerson* person;
         char* name;
         int age;
         g_type_init();
         person = demo_person_new();
         demo_person_set_name(person, "John");
         demo_person_set_age(person, 24);
         g_object_get(G_OBJECT(person), "name", &name,
                      "age", &age, NULL);
         printf("Hello, my name is %s, I'm %d years old.\n",
                 name, age);
         g_free(name);
         g_object_unref(G_OBJECT(person));
         return 0;
     }
5. write a makefile:
    CC = gcc
    CFLAGS = $(shell pkg-config --cflags gobject-2.0) -g
    LDFLAGS = $(shell pkg-config --libs gobject-2.0)

    all: demoperson-t

    demoperson-t: demoperson-t.o demoperson.o
    	$(CC) -o $@ $^ $(LDFLAGS)
6. make && ./demoperson-t

Enjoy!

See Also
http://www.gustavobarbieri.com.br/gobject-class.el

TODO
1. support for methods
2. create interface

Put this file into your load-path and the following into your ~/.emacs:
  (require 'gob)
