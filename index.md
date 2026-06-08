# site ![](reference/figures/Site.png)

> ## 📢 Advance notice of a major update
>
> The malariaverse site files and several supporting packages are about
> to receive a significant update that will change the site-file
> structure and may **break existing workflows**. **Please read
> [Upcoming changes to
> malariaverse](https://mrc-ide.github.io/site/articles/Upcoming-changes.html)**
> for what’s changing, timing, and how to keep reproducing existing
> work.

The site package supports malariaverse users to:

1.  Access and download the latest country site-files 🌐 ➡️ 💻
2.  Translate site file information into malariasimulation parameters 🌍
    ➡️ 📉

⚠️ The site package streamlines model runs for specific geographies, but
it also encapsulates many underlying assumptions, limitations, and
uncertainties. Mis-specifying inputs or misinterpreting outputs from the
complex malariasimulation model remains very possible. We strongly
recommend consulting a member of the Imperial College modelling team
before using these results for science, policy or decision-making
purposes.

While every effort has been made to ensure the reliability of the
package and its outputs, the authors, contributors, and affiliated
institutions accept no responsibility or liability for any errors,
omissions, or consequences arising from their use. All results should be
interpreted with caution and professional judgement.

Email: <malariaverse@imperial.ac.uk>

## The site-file

The site-file is the file storing all of the sub nationally
disaggregated information for a country. Components of the site-file
include:

1.  The
    [Metadata](https://mrc-ide.github.io/site/articles/Metadata.html),
    with high level information about the country and site-file version.
2.  [Historical Epidemiological
    Data](https://mrc-ide.github.io/site/articles/historical_epi.html)
    with estimates of cases, deaths and parasite prevalence.
3.  [Population and
    Demography](https://mrc-ide.github.io/site/articles/pop_demog.html)
    information.
4.  Estimates of the [Historical Intervention
    Use](https://mrc-ide.github.io/site/articles/Interventions.html) in
    the country.
5.  [Rainfalll and
    Seasonality](https://mrc-ide.github.io/site/articles/Seasonality.html)
    data.
6.  Information on the [Mosquito
    Vectors](https://mrc-ide.github.io/site/articles/vectors.html)

## Accessing and downloading site-files

Please see [this detailed
guide](https://mrc-ide.github.io/site/articles/Accessing-site-files.html)
for information on how to gain access and download site files.

## Translating site-file information to malariasimulation parameters

If we have access a site file, then all we need to do to run the model
is

1.  Isolate a single site, this is a single sub-national unit within the
    country
2.  Create the parameter list
3.  Pass that to malariasimulation to run the simulation

``` r


# Pull information for a single sub-national unit from the site-file
site <- subset_site(
  site = example_site,
  site_filter = data.frame(
  country = "Burkina Faso",
  iso3c = "BFA",
  name_1 = "Sahel",
  urban_rural = "rural")
  )

# Convert site information to malariasimulation parameters
site_par <- site_parameters(
  interventions = site$interventions,
  demography = site$demography,
  vectors = site$vectors$vector_species,
  seasonality = site$seasonality$seasonality_parameters,
  eir = site$eir$eir,
  overrides = list(
    human_population = 1000
  )
)

# Run the model!
site_sim <- malariasimulation::run_simulation(
  timesteps = site_par$timesteps,
  parameters = site_par
)
```

## Installation

You can install the development version of site from
[GitHub](https://github.com/) with:

``` r

# install.packages("pak")
pak::pak("mrc-ide/site")
```
