## Determine the Mohorovičić discontinuity layer in 1D layered Earth Model

The Mohorovičić discontinuity, usually referred to as the Moho discontinuity or the Moho, is the boundary between the Earth's crust and the mantle. It is defined by the distinct change in velocity of seismological waves as they pass through changing densities of rock. This distinctive discontinuous layer extends from 30 km to 50 km below the continents and about 10 km below sea-level in the ocean basins.

## Compile the code
Requires the installation of `gfortran` or add your compiler to the `makefile`.
```
make clean
make
```

## Run the code
1. Edit the 1D Earth model file name in the script `determineMohoLayer.f90`, line 5
```
character (len=256) :: model_file = 'model1D_prem.dat' !PREM
```

1. Compile the script
1. Run the script `./determineMohoLayer`
1. This will output the total layers in the model file, total number of discontinuities, and the layer number for the Moho discontinuity.
1. Edit the script according to your needs.

## References
1. https://www-nature-com.libproxy.berkeley.edu/articles/2071082a0
1. https://geology.com/articles/mohorovicic-discontinuity.shtml
1. https://en.wikipedia.org/wiki/Mohorovičić_discontinuity
