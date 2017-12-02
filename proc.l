func factorial ( val n ) begin 
 if n = 1 then begin 
  return 1 ; end  
 else begin 
  return n * factorial ( n - 1 ) ; 
 end ; 
end ; 
var z int = 10 ; 
z := factorial ( z ) ; 
print z ; 