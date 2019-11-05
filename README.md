# Reduce-Algebraic-Expressions

Given any algebraic expression, this Prolog code reduces it into it’s most simplified form.  
  
The query to simplify expressions can be invoked as:  
  
simplify(InExpr, OutExpr).  
  
Sample input/output:  
  
?- simplify( ((x+x)/x)\*(y+y-y), OutExpr).  
OutExpr = 2\*y   
  
?- simplify( (x*(y/y)-x)*x , OutExpr).  
OutExpr = 0  
  
?- simplify( (a\*b/1)*(c+1-c) , OutExpr).  
OutExpr = a\*b  
  
?- simplify(4\*a\*2\*b\*3+ 24+a+15+d, OutExpr).  
OutExpr = 39+a\*b\*24+a+d  
  
?- simplify((a+a)/a+a*(a-a),OutExpr).  
OutExpr = 2  
  
?- simplify(3*(5/2)+b,OutExpr).  
OutExpr = 7.5+b  
