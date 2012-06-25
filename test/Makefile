OUT=../js2sexp
CMD=time ruby
OPT=-q -x

all: test testpl 

test: $(OUT)
	$(CMD) $(OUT) jquery_nomin.js
	$(CMD) $(OUT) jquery.js
	$(CMD) $(OUT) _debugger.js
	$(CMD) $(OUT) _linklist.js
	$(CMD) $(OUT) assert.js
	$(CMD) $(OUT) buffer.js
	$(CMD) $(OUT) buffer_ieee754.js
	$(CMD) $(OUT) child_process.js
	$(CMD) $(OUT) cluster.js
	$(CMD) $(OUT) console.js
	$(CMD) $(OUT) constants.js
	$(CMD) $(OUT) crypto.js
	$(CMD) $(OUT) dgram.js
	$(CMD) $(OUT) dns.js
	$(CMD) $(OUT) domain.js
	$(CMD) $(OUT) events.js
	$(CMD) $(OUT) freelist.js
	$(CMD) $(OUT) fs.js
	$(CMD) $(OUT) http.js
	$(CMD) $(OUT) https.js
	$(CMD) $(OUT) module.js
	$(CMD) $(OUT) net.js
	$(CMD) $(OUT) os.js
	$(CMD) $(OUT) path.js
	$(CMD) $(OUT) punycode.js
	$(CMD) $(OUT) querystring.js
	$(CMD) $(OUT) readline.js
	$(CMD) $(OUT) repl.js
	$(CMD) $(OUT) stream.js
	$(CMD) $(OUT) string_decoder.js
	$(CMD) $(OUT) sys.js
	$(CMD) $(OUT) t.js
	$(CMD) $(OUT) timers.js
	$(CMD) $(OUT) tls.js
	$(CMD) $(OUT) tty.js
	$(CMD) $(OUT) url.js
	$(CMD) $(OUT) util.js
	$(CMD) $(OUT) vm.js
	$(CMD) $(OUT) zlib.js

hoge: test
