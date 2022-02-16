Read me file for inflation RB code.
Sean Langcake 
Feb 2022

The code replicates Mahedy & Shapiro https://www.frbsf.org/economic-research/files/el2017-35.pdf?mod=article_inline

My RB from June 2021: https://www.oxfordeconomics.com/publication/open/355428

In its current form, the code will run using Australian data - but you will need to change some user inputs and ensure the relevant R packages are installed.

R packages you will need: https://github.com/slangcake/oxgraphs 
	(Running the ox_setup() command at line 15 will take care of installing most other required packages)
	You'll need 'car' and 'rlist' - install with the following command
		install.packages(c('car','rlist'))

Options at line 30 allow you to chose the model specifications you want to test. Select at least one - estimation will loop over each combination of options:
	gp_c allows you to test a linear [UP-NAIRU] and/or non-linear [(UP-NAIRU)/NAIRU] unemployment gap
	tp_c test different exchange rate/import price measures 
	sig_c is the significance level for the hypothesis tests
	ie includes/excludes the inflation expectations term

Data import at line 164 is the seasonally adjusted index for each component

Data import at line 167 are the index point contributions for each component. These are used to calculate the 'effective weight' of each component

clean_names object at line 182 gives you readable names for each CPI component

gem data read at line 217 requires installation of the mdl tool - Stefan Angrick's github page has a link to the install instructions. You will need to specify the sector you want to import variables for.

ie data read at line 221 is your preferred inflation expectations measure. Have included the Aus data as an RDS so you can see the format

Data selection at line 227 will need tweaking depending on country you have imported GEM data for. You will need to add your country name as a suffix

Dummies are added at line 274. May require a tweak to the pc_est function if you're testing out different estimation samples that drop one of your dummies (see line 70)

Lines 330-331 - some Aus specific data cleaning 

Estimation and results finish at line 393. Line 396 creates an html file to look at different model specifications. The remainder of the code produces charts I used in my RB