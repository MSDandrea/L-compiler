var x int = 120 ; 
var y int = 1 ;  
const z int = 1 ; 
while ~ x = 0 do begin
 print x  ; 
 y := y * x ; 
 x := x - z ; 
end ; 
const k int = 2 ; 
if y = 0 then 
 begin
  print k ; 
 end 
else
 begin 
  print k + 1 ; 
 end ;