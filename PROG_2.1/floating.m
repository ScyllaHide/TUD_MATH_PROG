function floating

clear all
close all

normalized=[1/4  
  17/64 
  9/32 
  19/64 
  5/16 
  21/64 
  11/32 
  23/64 
  3/8  
  25/64 
  13/32 
  27/64 
  7/16 
  29/64 
  15/32 
  31/64 
  1/2  
  17/32 
  9/16 
  19/32 
  5/8  
  21/32 
  11/16 
  23/32 
  3/4  
  25/32 
  13/16 
  27/32 
  7/8  
  29/32 
  15/16 
  31/32 
  1        
  1+1/16 
  1+1/8  
  1+3/16 
  1+1/4  
  1+5/16 
  1+3/8  
  1+7/16 
  1+1/2  
  1+9/16 
  1+5/8  
  1+11/16 
  1+3/4  
  1+13/16 
  1+7/8  
  1+15/16 
  2        
  2+1/8  
  2+1/4  
  2+3/8  
  2+1/2  
  2+5/8  
  2+3/4  
  2+7/8  
  3        
  3+1/8  
  3+1/4  
  3+3/8  
  3+1/2  
  3+5/8  
  3+3/4  
  3+7/8];


denorm=[0        
  1/64 
  1/32 
  3/64 
  1/16 
  5/64 
  3/32 
  7/64 
  1/8  
  9/64 
  5/32 
  11/64 
  3/16 
  13/64 
  7/32 
  15/64];


other=[1/32 
  1/16 
  3/32 
  1/8  
  5/32 
  3/16 
  7/32 
  1/4  
  9/32 
  5/16 
  11/32 
  3/8  
  13/32 
  7/16 
  15/32 
  1/2  
  9/16 
  5/8  
  11/16 
  3/4  
  13/16 
  7/8  
  15/16 
  1        
  1+1/8  
  1+1/4  
  1+3/8  
  1+1/2  
  1+5/8  
  1+3/4  
  1+7/8];  

  a=length(normalized');
b=length(denorm');
c=length(other');

ynorm=zeros(a,1);
ydenorm=zeros(b,1);
yother=zeros(c,1);

hold on
set(gca,'ylim',[-0.5 0.5],'YTickLabel',[],'YTick',[])
set(gca,'xlim',[0 4],'XTick',[0:0.25:4])

plot(normalized,ynorm,"rx")
plot(denorm, ydenorm, "gx")
plot(other,yother, "bx")

legend({'normalisiert','denormalisiert','weitere'},'Location')

%print(figure(1),'-depsc','-r600','testname.png');
%set(gcf, 'PaperSize',[15 5]);

%hgexport(fig,filename)

set output abbildung.eps

end