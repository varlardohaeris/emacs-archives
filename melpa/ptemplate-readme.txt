Creating projects can be a lot of work. Cask files need to be set up, a
License file must be added, maybe build system files need to be created. A
lot of that can be automated, which is what ptemplate does. You can create a
set of templates categorized by type/template like in eclipse, and ptemplate
will then initialize the project for you. In the template you can have any
number of yasnippets or normal files.

Security note: yasnippets allow arbitrary code execution, as do
.ptemplate.el files. DO NOT EXPAND UNTRUSTED TEMPLATES. ptemplate DOES NOT
make ANY special effort to protect against malicious templates.
