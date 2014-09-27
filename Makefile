PROJECT = erl_reader

DEPS = atomizer seymour uuid
dep_atomizer = git https://github.com/grahamrhay/atomizer master
dep_seymour = git https://github.com/grahamrhay/seymour master
dep_uuid = git https://github.com/okeuday/uuid v1.3.1

include erlang.mk
