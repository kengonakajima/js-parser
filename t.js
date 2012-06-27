/*
a = - b--;

var t={ a:1, b:"2", c:1+2 };

/*var t={}

/*


/*
var tt={};
tt.a = 1;
tt.b = 2;
tt["a"] = 3;



/*
 
 var a=2;
a=0.5
a=0x12
a=012
a=7e+8
a=7E+8
a=7e-8
a=0.7e-8
a=19.7e-8
a=-19.7e+8


/*

 
var a="aho";

a="aho\""
a='aho'
a='aho\''
a='a"ho'
a="a'ho"
a="a'hoほげ"
a="aho\012aho"
a="aho\b\n\t\r\faho"

/*
 
var a="hoge1\
hoge2\
hoge3"





/*
out != 2

out += 2
out += "2"
out %= 2
out ^= 2
out /= 2
out *= 2
out -= 2
out += 2
out <<= 2
out >>= 2
out &= 2
out |= 2




/*
 
//a.b = function(q) {
a = function(q) {
}
c = function b(q) {}

/*
 
 function hoge(a,b,c){
  return 2
}



/*
 function noarg1() {
}
function noarg2() {
  return
}
function noarg3() {
  return 2
}


//for(var i in [1,2,3]){}

/*
 var t=[1,2,3]


 for(i in [1,2,3]){
}
//for(var b=1;b<4;b++){}

/*
for(;a<1;){}

 
for(a=1;a<2;){}
/*

for( a=1;a<4;a++){
    ;
}



    /*
for(a=1;;){}
for(;b<2;){}
for(;b<2;c++){}
for(a=1;;a++){}
for(a=1;a<2;){}
for(;;){}

/*


//if(1){;;;}

/*
//if(a){ 1; } else { 2; }

if(2){
    3;
} else if(4){
    5;
} else if(6){
    7;
} else {
    8;
}



 do {
    do
        console.log();
    while(false);
} while(true);



hoge : {
for(;;){
    break hoge;
    continue;
}
}






//out = - a++;



//out= !a || (!true)
//out= ((!a) && (!true))

//out= !a && !true
//out= a && !true
//out= a && true

 /*
 out= !a
out= ~a
out= typeof(a)
out= typeof a



out!=a

/*
 var a=[1,2,]
// var a=[10,20,30]

/*
 function a(){
    l1: 9;
    l2: 10;    
}

/*
 switch(a){
case 1:
    2;
case 3:
    4;
default:
    5;
    6;
case 7:
    function b() { };
}

/*
 
var a=1,b=2;

//throw "eee";

/*
 try {
} catch(e){
}finally {
}
    


/*
 {
    {
    }
    {
        var a;
    }
}


/*
 var b=1;
b+=2;
b++;

/*
var a;
function a(b) {
    b=2;
}

/*
var q;
a,b=1,2;
var a=1;
function a() {
}
function b(c) {
}
var v=function(c,d,e) {
    var aa=1;
    var bb="aa";
}
if(true) {
} else if(false) {
    var a = new Array(1,2,3);
}
b.c.d = e;
qq={};



/*
Agent.prototype.createSocket = function(name, host, port, localAddress) {

  var self = this;
  var options = util._extend({}, self.options);
  options.port = port;
  options.host = host;
  options.localAddress = localAddress;
  var s = self.createConnection(options);
  if (!self.sockets[name]) {
    self.sockets[name] = [];
  }
  this.sockets[name].push(s);
  var onFree = function() {
    self.emit('free', s, host, port, localAddress);
  }
  s.on('free', onFree);
  var onClose = function(err) {
    self.removeSocket(s, name, host, port, localAddress);
  }
  s.on('close', onClose);
  var onRemove = function() {
    self.removeSocket(s, name, host, port, localAddress);
    s.removeListener('close', onClose);
    s.removeListener('free', onFree);
    s.removeListener('agentRemove', onRemove);
  }
  s.on('agentRemove', onRemove);
  return s;

};

function p() {
    var a=1;
};


if (a && b) {
};

var a=1;
*/