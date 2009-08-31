all: compile

compile:
	@mkdir -p ebin
	@erl -pa ebin ../epitest/ebin -noshell -s make all -s erlang halt

clean:
	rm -rf ebin/*.beam examples/*.beam


test: all
	@mkdir -p _tests
	@erl -noshell -epitest dir \"_tests\" -sname epitest -pa t ebin ../epitest/ebin -s epitest -s epitest modules tests -s epitest_console_logger -s epitest run 
