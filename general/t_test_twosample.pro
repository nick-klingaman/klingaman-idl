FUNCTION T_TEST_TWOSAMPLE,X1,S1,N1,X2,S2,N2,P

df = (S1/n1 + S2/n2)^2 / ((S1/n1)^2/(n1-1)+(S2/n2)^2/(n2-1))*3

s = SQRT((S1/n1)+(S2/n2))
t = (X1-X2)/s

t_critical = T_CVF(P,df)

IF (t gt t_critical) THEN BEGIN
    score=1
ENDIF ELSE $
  score=0

RETURN,score

END
