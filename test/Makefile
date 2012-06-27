OUT=../js2sexp
CMD=time ruby
OPT=-q -x

all: test testbig


test: $(OUT)
	$(CMD) $(OUT) call.js
	$(CMD) $(OUT) shebang.js
	$(CMD) $(OUT) new.js
	$(CMD) $(OUT) op3.js
	$(CMD) $(OUT) asgn.js
	$(CMD) $(OUT) postfix.js
	$(CMD) $(OUT) regex.js
	$(CMD) $(OUT) block.js
	$(CMD) $(OUT) try.js
	$(CMD) $(OUT) throw.js
	$(CMD) $(OUT) sw.js
	$(CMD) $(OUT) label.js
	$(CMD) $(OUT) ary.js
	$(CMD) $(OUT) bool.js
	$(CMD) $(OUT) comment.js
	$(CMD) $(OUT) cont.js
	$(CMD) $(OUT) do.js
	$(CMD) $(OUT) elseif.js
	$(CMD) $(OUT) empty3.js
	$(CMD) $(OUT) empty2.js
	$(CMD) $(OUT) empty.js
	$(CMD) $(OUT) for3.js
	$(CMD) $(OUT) forin.js
	$(CMD) $(OUT) func.js
	$(CMD) $(OUT) func2.js
	$(CMD) $(OUT) hello.js
	$(CMD) $(OUT) ifthen.js
	$(CMD) $(OUT) lcomment.js
	$(CMD) $(OUT) let.js
	$(CMD) $(OUT) long.js
	$(CMD) $(OUT) mini.js
	$(CMD) $(OUT) mlstr.js
	$(CMD) $(OUT) nstr.js
	$(CMD) $(OUT) num.js
	$(CMD) $(OUT) obj.js
	$(CMD) $(OUT) ops.js
	$(CMD) $(OUT) ret.js
	$(CMD) $(OUT) ret2.js
	$(CMD) $(OUT) semi.js
	$(CMD) $(OUT) unops.js
	$(CMD) $(OUT) while.js




testbig: $(OUT)
	$(CMD) $(OUT) ./big/jquery_nomin.js
#	$(CMD) $(OUT) ./big/jquery.js
	$(CMD) $(OUT) ./big/_debugger.js
	$(CMD) $(OUT) ./big/_linklist.js
	$(CMD) $(OUT) ./big/assert.js
	$(CMD) $(OUT) ./big/buffer.js
	$(CMD) $(OUT) ./big/buffer_ieee754.js
	$(CMD) $(OUT) ./big/child_process.js
	$(CMD) $(OUT) ./big/cluster.js
	$(CMD) $(OUT) ./big/console.js
	$(CMD) $(OUT) ./big/constants.js
	$(CMD) $(OUT) ./big/crypto.js
	$(CMD) $(OUT) ./big/dgram.js
	$(CMD) $(OUT) ./big/dns.js
	$(CMD) $(OUT) ./big/domain.js
	$(CMD) $(OUT) ./big/events.js
	$(CMD) $(OUT) ./big/freelist.js
	$(CMD) $(OUT) ./big/fs.js
	$(CMD) $(OUT) ./big/http.js
	$(CMD) $(OUT) ./big/https.js
	$(CMD) $(OUT) ./big/module.js
	$(CMD) $(OUT) ./big/net.js
	$(CMD) $(OUT) ./big/os.js
	$(CMD) $(OUT) ./big/path.js
	$(CMD) $(OUT) ./big/punycode.js
	$(CMD) $(OUT) ./big/querystring.js
	$(CMD) $(OUT) ./big/readline.js
	$(CMD) $(OUT) ./big/repl.js
	$(CMD) $(OUT) ./big/stream.js
	$(CMD) $(OUT) ./big/string_decoder.js
	$(CMD) $(OUT) ./big/sys.js
	$(CMD) $(OUT) ./big/t.js
	$(CMD) $(OUT) ./big/timers.js
	$(CMD) $(OUT) ./big/tls.js
	$(CMD) $(OUT) ./big/tty.js
	$(CMD) $(OUT) ./big/url.js
	$(CMD) $(OUT) ./big/util.js
	$(CMD) $(OUT) ./big/vm.js
	$(CMD) $(OUT) ./big/zlib.js

hoge: test
