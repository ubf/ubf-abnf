

#Universal Binary Format and ABNF#

<pre>This is ubf-abnf, a framework for integrating UBF and ABNF.  This
repository depends on the ubf and abnfc open source repositories.


Quick Start Recipe
==================

To download, build, and test the ubf_abnf application in one shot,
please follow this recipe:

    $ mkdir working-directory-name
    $ cd working-directory-name
    $ git clone git://github.com/norton/ubf-abnf.git ubf_abnf
    $ cd ubf_abnf
    $ ./rebar get-deps
    $ ./rebar clean
    $ ./rebar compile
    $ ./rebar eunit

For an alternative recipe with other "features" albeit more complex,
please read further.


To download
===========

1. Configure your e-mail and name for Git

    $ git config --global user.email "you@example.com"
    $ git config --global user.name "Your Name"

2. Install Repo

    $ mkdir -p ~/bin
    $ wget -O - https://github.com/android/tools_repo/raw/master/repo > ~/bin/repo
    $ perl -i.bak -pe 's!git://android.git.kernel.org/tools/repo.git!git://github.com/android/tools_repo.git!;' ~/bin/repo
    $ chmod a+x ~/bin/repo

    CAUTION: Since access to kernel.org has been shutdown due to
    hackers, fetch and replace repo tool with android's GitHub
    repository mirror.

3. Create working directory

    $ mkdir working-directory-name
    $ cd working-directory-name
    $ repo init -u git://github.com/norton/manifests.git -m ubf-abnf-default.xml

    NOTE: Your "Git" identity is needed during the init step.  Please
    enter the name and email of your GitHub account if you have one.
    Team members having read-write access are recommended to use "repo
    init -u git@github.com:norton/manifests.git -m
    ubf-abnf-default-rw.xml".

    TIP: If you want to checkout the latest development version of UBF
    ABNF, please append " -b dev" to the repo init command.

4. Download Git repositories

    $ cd working-directory-name
    $ repo sync

For futher information and help for related tools, please refer to the
following links:

- Erlang - http://www.erlang.org/
  * *R13B04 or newer, R14B03 has been tested most recently*
- Git - http://git-scm.com/
  * *Git 1.5.4 or newer, Git 1.7.6.1 has been tested recently*
  * _required for Repo and GitHub_
- GitHub - https://github.com
- Python - http://www.python.org
  * *Python 2.4 or newer, Python 2.7.1 has been tested most recently
     (CAUTION: Python 3.x might be too new)*
  * _required for Repo_
- Rebar - https://github.com/basho/rebar/wiki
- Repo - http://source.android.com/source/git-repo.html


To build - basic recipe
=======================

1. Get and install an erlang system
   http://www.erlang.org

2. Build UBF
   $ cd working-directory-name/src
   $ make compile

3. Run the unit tests
   $ cd working-directory-name/src
   $ make eunit


To build - optional features
============================

A. Dialyzer Testing _basic recipe_

   A.1. Build Dialyzer's PLT _(required once)_

   $ cd working-directory-name/src
   $ make build-plt

   TIP: Check Makefile and dialyzer's documentation for further
   information.

   A.2. Dialyze with specs

   $ cd working-directory-name/src
   $ make dialyze

   CAUTION: If you manually run dialyzer with the "-r" option, execute
   "make clean compile" first to avoid finding duplicate beam files
   underneath rebar's .eunit directory.  Check Makefile for further
   information.

   A.3. Dialyze without specs

   $ cd working-directory-name/src
   $ make dialyze-nospec


Documentation -- Where should I start?
======================================

This README is a good first step.  Check out and build using the "To
build" instructions above.

ABNF samples and eunit tests can be found in the test/eunit directory.
directory.


What is UBF?
============

UBF is the "Universal Binary Format", designed and implemented by Joe
Armstrong.  UBF is a language for transporting and describing complex
data structures across a network.  It has three components:

   * UBF(A) is a "language neutral" data transport format, roughly
     equivalent to well-formed XML.

   * UBF(B) is a programming language for describing types in UBF(A)
     and protocols between clients and servers.  This layer is
     typically called the "protocol contract".  UBF(B) is roughly
     equivalent to Verified XML, XML-schemas, SOAP and WDSL.

   * UBF(C) is a meta-level protocol used between a UBF client and a
     UBF server.

See http://norton.github.com/ubf for further details.


What is ABNF?
=============

Augmented Backus-Naur Form (ABNF) is a metalanguage based on
Backus-Naur Form (BNF), but consisting of its own syntax and
derivation rules.

   * The motive principle for ABNF is to describe a formal system of a
     language to be used as a bidirectional communications protocol.

   * It is defined by Internet Standard 68 (STD 68), which as of May
     2008 is RFC 5234, and it often serves as the definition language
     for IETF communication protocols.


Credits
=======

Many, many thanks to Joe Armstrong, UBF's designer and original
implementor.

Thanks to Anders Nygren, ABNFC's designer and implementor.  ABNFC is a
parser generator for Erlang.  UBF-ABNF relies on the ABNFC application
for parsing ABNF into a symbolic form.

Gemini Mobile Technologies, Inc. has approved the release of this
repository under an MIT license.</pre>.
<pre>This is ubf-abnf, a framework for integrating UBF and ABNF.  This
repository depends on the ubf and abnfc open source repositories.


Quick Start Recipe
==================

To download, build, and test the ubf_abnf application in one shot,
please follow this recipe:

    $ mkdir working-directory-name
    $ cd working-directory-name
    $ git clone git://github.com/norton/ubf-abnf.git ubf_abnf
    $ cd ubf_abnf
    $ ./rebar get-deps
    $ ./rebar clean
    $ ./rebar compile
    $ ./rebar eunit

For an alternative recipe with other "features" albeit more complex,
please read further.


To download
===========

1. Configure your e-mail and name for Git

    $ git config --global user.email "you@example.com"
    $ git config --global user.name "Your Name"

2. Install Repo

    $ mkdir -p ~/bin
    $ wget -O - https://github.com/android/tools_repo/raw/master/repo > ~/bin/repo
    $ perl -i.bak -pe 's!git://android.git.kernel.org/tools/repo.git!git://github.com/android/tools_repo.git!;' ~/bin/repo
    $ chmod a+x ~/bin/repo

    CAUTION: Since access to kernel.org has been shutdown due to
    hackers, fetch and replace repo tool with android's GitHub
    repository mirror.

3. Create working directory

    $ mkdir working-directory-name
    $ cd working-directory-name
    $ repo init -u git://github.com/norton/manifests.git -m ubf-abnf-default.xml

    NOTE: Your "Git" identity is needed during the init step.  Please
    enter the name and email of your GitHub account if you have one.
    Team members having read-write access are recommended to use "repo
    init -u git@github.com:norton/manifests.git -m
    ubf-abnf-default-rw.xml".

    TIP: If you want to checkout the latest development version of UBF
    ABNF, please append " -b dev" to the repo init command.

4. Download Git repositories

    $ cd working-directory-name
    $ repo sync

For futher information and help for related tools, please refer to the
following links:

- Erlang - http://www.erlang.org/
  * *R13B04 or newer, R14B03 has been tested most recently*
- Git - http://git-scm.com/
  * *Git 1.5.4 or newer, Git 1.7.6.1 has been tested recently*
  * _required for Repo and GitHub_
- GitHub - https://github.com
- Python - http://www.python.org
  * *Python 2.4 or newer, Python 2.7.1 has been tested most recently
     (CAUTION: Python 3.x might be too new)*
  * _required for Repo_
- Rebar - https://github.com/basho/rebar/wiki
- Repo - http://source.android.com/source/git-repo.html


To build - basic recipe
=======================

1. Get and install an erlang system
   http://www.erlang.org

2. Build UBF
   $ cd working-directory-name/src
   $ make compile

3. Run the unit tests
   $ cd working-directory-name/src
   $ make eunit


To build - optional features
============================

A. Dialyzer Testing _basic recipe_

   A.1. Build Dialyzer's PLT _(required once)_

   $ cd working-directory-name/src
   $ make build-plt

   TIP: Check Makefile and dialyzer's documentation for further
   information.

   A.2. Dialyze with specs

   $ cd working-directory-name/src
   $ make dialyze

   CAUTION: If you manually run dialyzer with the "-r" option, execute
   "make clean compile" first to avoid finding duplicate beam files
   underneath rebar's .eunit directory.  Check Makefile for further
   information.

   A.3. Dialyze without specs

   $ cd working-directory-name/src
   $ make dialyze-nospec


Documentation -- Where should I start?
======================================

This README is a good first step.  Check out and build using the "To
build" instructions above.

ABNF samples and eunit tests can be found in the test/eunit directory.
directory.


What is UBF?
============

UBF is the "Universal Binary Format", designed and implemented by Joe
Armstrong.  UBF is a language for transporting and describing complex
data structures across a network.  It has three components:

   * UBF(A) is a "language neutral" data transport format, roughly
     equivalent to well-formed XML.

   * UBF(B) is a programming language for describing types in UBF(A)
     and protocols between clients and servers.  This layer is
     typically called the "protocol contract".  UBF(B) is roughly
     equivalent to Verified XML, XML-schemas, SOAP and WDSL.

   * UBF(C) is a meta-level protocol used between a UBF client and a
     UBF server.

See http://norton.github.com/ubf for further details.


What is ABNF?
=============

Augmented Backus-Naur Form (ABNF) is a metalanguage based on
Backus-Naur Form (BNF), but consisting of its own syntax and
derivation rules.

   * The motive principle for ABNF is to describe a formal system of a
     language to be used as a bidirectional communications protocol.

   * It is defined by Internet Standard 68 (STD 68), which as of May
     2008 is RFC 5234, and it often serves as the definition language
     for IETF communication protocols.


Credits
=======

Many, many thanks to Joe Armstrong, UBF's designer and original
implementor.

Thanks to Anders Nygren, ABNFC's designer and implementor.  ABNFC is a
parser generator for Erlang.  UBF-ABNF relies on the ABNFC application
for parsing ABNF into a symbolic form.

Gemini Mobile Technologies, Inc. has approved the release of this
repository under an MIT license.</pre>


##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/norton/ubf-abnf/blob/master/doc/abnf_contract_parser.md" class="module">abnf_contract_parser</a></td></tr></table>

