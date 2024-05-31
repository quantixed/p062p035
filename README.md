# p062p035

Data and code for manuscript `p062p035`

**Structural characterization and inhibition of the interaction between ch-TOG and TACC3**

James Shelford\*, Selena G. Burgess\*, Elena Rostkova, Mark W. Richards, Christian Tiede, Alistair J. Fielding, Tina Daviter, Darren C. Tomlinson, Antonio N. Calabrese, Mark Pfuhl\*\*, Richard Bayliss\*\* and Stephen J. Royle\*\*

*bioRxiv* 2024. DOI: [https://doi.org/10.1101/2024.05.31.596836](https://doi.org/10.1101/2024.05.31.596836)

## Code and data for recreation of plots

### Main Figures

- mCherry-Affimer IP quantification - `IP_Quantification`
- Spindle ch-TOG Quantification - `Spindle_Quantification`
- Pericentrin quantification - `Pericentrin_quant`
- Centrin-pericentrin quantification - `Centrin-pericentrin`
- ch-TOG mutant pericentrin - `chTOG_mut_pcnt`
- Sankey diagram - `Sankey`
- Mitotic timing - `MitoticTiming`


### Supplementary Figures

- 3D Mitotic spindle analytics - `SpindleAnalytics`
- Affimer knocksideways - `JS155`

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
