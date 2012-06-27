for(var i in [1,2,3]){}

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