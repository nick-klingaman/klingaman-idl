PRO test_gaussian

nx = 100
ny = 100
constant = 0
amplitude = 1
xwidth = nx/4.
ywidth = nx/4.
xcenter = nx/2.
ycenter = nx/2.

x = findgen(nx) # replicate(1.0,ny)
y = replicate(1.0,nx) # findgen(ny)

u = ((x-xcenter)/xwidth)^2 + ((y-ycenter)/ywidth)^2

z = constant + amplitude * exp(-u/2)

surface,z

STOP

END
