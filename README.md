# p062p035

Data and code for manuscript `p062p035`

**Structural characterization and inhibition of the interaction between ch-TOG and TACC3**

James Shelford\*, Selena G. Burgess\*, Elena Rostkova, Mark W. Richards, Gabrielle Larocque, Josephina Sampson, Christian Tiede, Alistair J. Fielding, Tina Daviter, Darren C. Tomlinson, Antonio N. Calabrese, Mark Pfuhl\*\*, Richard Bayliss\*\* and Stephen J. Royle\*\*

*J Cell Biol* (2025) 224: e202407002, doi: [10.1083/jcb.202407002](https://doi.org/10.1083/jcb.202407002).


## Code and data for recreation of plots

### Main Figures

- mCherry-Affimer IP quantification - `IP_Quantification`
- Spindle ch-TOG quantification - `Spindle_Quantification`
- Spindle tubulin quantification - `gl_tub`
- Pericentrin quantification - `Pericentrin_quant`
- Centrin-pericentrin quantification - `Centrin-pericentrin`
- ch-TOG mutant pericentrin - `chTOG_mut_pcnt`
- Sankey diagram - `Sankey`
- Mitotic timing - `MitoticTiming`


### Supplementary Figures

- 3D Mitotic spindle analytics - `SpindleAnalytics`
- Affimer knocksideways - `JS155`
- Clathrin/TACC3 quantification - `gl_clat`
- PCNT foci with nocodazole - `gl_noc`

### Notes

The R/Igor code can be executed with data placed into the `Data` folder of the RProject.

Fiji scripts for image analysis are included in the `Script` folder of the respective project.

Raw data (images) are not included for space reasons, only text outputs from image analysis steps.

## Development

Some code in this repo was developed elsewhere or is still under development.
The version in this repo reflects the state at the time the figures were assembled.

[`https://github.com/james-shelford/Image_Analysis_Projects`](https://github.com/james-shelford/Image_Analysis_Projects)

[`https://github.com/james-shelford/SpindleAnalytics`](https://github.com/james-shelford/SpindleAnalytics)

[`https://github.com/quantixed/MitoticTiming`](https://github.com/quantixed/MitoticTiming)

## Contributions

R Code and ImageJ Scripts written by James Shelford.
Code review and small changes for publication by Stephen Royle.
Igor code by Stephen Royle.
