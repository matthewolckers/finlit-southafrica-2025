# Financial Literacy in South Africa
This repo contains the Stata code produced by [Matthew Olckers](http://www.matthewolckers.com/) and [Elizabeth Nanziri](https://sites.google.com/view/elnanziri) for "Financial Literacy in South Africa", a paper analysing the financial literacy module of the [National Income Dynamic Study (NIDS)](http://nids.uct.ac.za/). NIDS is the first national panel study of individuals and households in South Africa. 

In 2016, Matthew approached NIDS to include five questions formulated by [Annamaria Lusardi](https://business.gwu.edu/annamaria-lusardi) and [Olivia Mitchell](https://bepp.wharton.upenn.edu/profile/mitchelo/) which provide a measure of financial literacy. These questions have been included in [many household surveys worldwide](http://gflec.org/initiatives/flat-world/). NIDS included the questions in Wave 5 of the survey in the following format:
- Suppose you need to borrow R100. Which is the lower amount to pay back: R105 or R100 plus three percent?
- Suppose over the next 10 years the prices of the things you buy double. If your income also doubles, will you be able to buy less than you can buy today, the same as you can buy today, or more than you can buy today?
- Suppose you put money in the bank for two years and the bank agrees to add 15 percent per year to your account. Will the bank add more money to your account the second year than it did the first year, or will it add the same amount of money both years?
- Suppose you had R100 in a savings account and the bank adds 10 percent per year to the account. After five years, if you did not remove any money from the account, would you have more than R150, exactly R150, less then R150?
- Suppose you have some money. Is it safer to put your money into one business or investment, or to put your money into multiple businesses or investments?


## Step 1: Download NIDS Wave 5

**Repository:** [DataFirst Open Data Portal](https://www.datafirst.uct.ac.za/dataportal/index.php/catalog/central)
**Dataset:** National Income Dynamics Study (NIDS) - Wave 5 (2017)

### Download Procedure

1.  **Authentication:** Log in to your DataFirst account.
2.  **Locate Study:** Navigate to the Data Catalog and search for **"NIDS Wave 5"**.
3.  **Access:** Click the **Get Microdata** tab and select the **Stata** dataset format.
4.  **Declaration:** Complete the intended use form to authorize the download.

### Directory Structure
Unzip the downloaded archive and move the resulting files into the following relative path within your project directory:

`data/raw/`

### File Manifest
Verify that the following `.dta` files are present in `data/raw/`:

* `Admin_W5_Anon_V1.0.0.dta`
* `Adult_W5_Anon_V1.0.0.dta`
* `Child_W5_Anon_V1.0.0.dta`
* `hhderived_W5_Anon_V1.0.0.dta`
* `HHQuestionnaire_W5_Anon_V1.0.0.dta`
* `HouseholdRoster_W5_Anon_V1.0.0.dta`
* `indderived_W5_Anon_V1.0.0.dta`
* `Link_File_W5_Anon_V1.0.0.dta`
* `Proxy_W5_Anon_V1.0.0.dta`

## Step 2: Run the code
