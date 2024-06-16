proc phreg data=mydata;
class edema (param=reference ref=first);
model time*status(0)=  age albumin edema protime bili /risklimits;  
run;
