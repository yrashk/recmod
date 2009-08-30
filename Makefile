all: compile

compile:
	@mkdir -p ebin
	@erl -pa ebin -noshell -s make all -s erlang halt

clean:
	rm -rf ebin/*.beam examples/*.beam
