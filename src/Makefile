#
# $Id: Makefile 131597 2009-04-09 06:34:51Z norton $
#


MODULES = \
	abnf_contract_parser

EUNITTESTS = \
	abnf_rfc5234_core \
	abnf_rfc3986 \
	ubf_abnf_samples_test

MY_UBF_EBIN = ./example-symlink-to-ubf-ebin
MY_UBF_INCLUDE = ./example-symlink-to-ubf-include

MY_ABNFC_EBIN = ./example-symlink-to-abnfc-ebin
MY_ABNFC_INCLUDE = ./example-symlink-to-abnfc-include

# include erl make
include ../priv/make/erl_make.mk

ERL_COMPILE_FLAGS += \
	-I $(MY_ABNFC_INCLUDE) \
	-I $(MY_UBF_INCLUDE) \
	-pz $(MY_ABNFC_EBIN) \
	-pz $(MY_UBF_EBIN)

ERL_FLAGS += \
	-pz $(MY_UBF_EBIN) \
	-pz $(MY_ABNFC_EBIN)

# standard targets
check: all
	-rm -f ./*.log
	$(RUNERL1) -noinput -noshell -pz $(EUNITTEST_DIR) -pz $(MY_UBF_EBIN) \
		-s ubf_abnf_samples_test do_eunit \
		-s erlang halt \
		> ./check.log
