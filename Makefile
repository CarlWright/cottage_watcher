PROJECT = cottage_watcher

LOCAL_DEPS = cottage_alarm cottage_beacon

DEPS = bmp085 erlang_ale
dep_bmp085 = git  https://github.com/CarlWright/bmp085.git e2a5ee3
dep_erlang_ale = git https://github.com/esl/erlang_ale.git 92174ae

include erlang.mk


