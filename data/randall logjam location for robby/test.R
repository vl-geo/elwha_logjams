d = c("year","seg")
b = data.frame('1999','LR')
names(b) = d
c = data.frame('2000','LR')
names(c) = d
b = rbind(b,c)

y = c('1999', '2000','2001')
print(y[1])
for (x in 1:3){ 
  print(y[x])
  if (!((y[x] %in% b$year) & ('MR' %in% b$seg))){
    b2 = data.frame(y[x],'MR')
    names(b2) = d
    b = rbind(b,b2)
    print(b)
  }}


#Filenames to parse
