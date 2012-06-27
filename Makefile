OUT=js2sexp

all: $(OUT)

fulltest : 
	make -C test

$(OUT):  js.y inner.rb footer.rb header.rb
	racc -g -E -l js.y -o $(OUT) --executable=/usr/bin/ruby

hoge: $(OUT)
	ruby js2sexp -x t.js




clean:
		rm $(OUT)

