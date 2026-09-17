The code and content for the Forest Ecohydrology and Watershed Science Lab website.

**Note:** This [website](https://fews-lab.github.io/) is currently under construction. Please see the old website version: https://fews.forestry.oregonstate.edu/.

## Managing Publications on the Website

Publications are added to the website via a shared Zotero [group](https://www.zotero.org/groups/6675700/fews-publications). To update the citations follow the directions below to correctly add a new publication:

1.  **Ensure you have the required tools. You should only need to do this once:**

    -   Zotero installed on your computer

    -   Install [Better BibTeX for Zotero](https://retorque.re/zotero-better-bibtex/installation/) and enable within Zotero.

    -   Store your Zotero user ID as an environmental variable:

        -   Open your R Environment file

            ```{r}
            usethis::edit_r_environ()
            ```

        -   Add the text: `ZOTERO_USER_ID = "<ID HERE>"`. Your user ID can be found by logging into Home \> Settings \> Security. You ID will be shown under **Applications**.

        -   Restart R

    -   Have access to the **FEWS-Publications** Zotero

2.  **Add a citation**. The easiest way to do this is by using the browser extension to add a publication automatically, but they can also be added manually.

    -   If you add a publication manually, update the **Citation Key** at the top of the citation. Use the first author's last name, followed by the three first main words of the title starting in caps, followed the by the publication date (i.e., BladonWildfireImpactsStreams2026). Use a letter follwing for non-unique keys.

3.  **Link a PDF attachment.** If not automatically added, add a PDF copy of the publication if available by right clicking on the item and clicking **Add** **Attachments** and **File**.

4.  **Add tags**. Tags are used to assign additional information to publications the following tags are currently supported by the code:

    -   **in-review**: the publication is in review and limited information will be shown for the publication

    -   **primary**: receives a star in the publications

    -   **informal**: indicates a non-peer reviewed publication and is put in a different section on the website.

5.  **Add links for additional materials.** To link pre-prints or additional materials (like code, data packages, etc.), add it to the **Extra** field using the form.

    -   Materials: `<LINK>`

    -   Preprint: `<LINK>`

    Currently only one link per field is supported. If you use both separate them with a new line.

6.  **In the code repository, re-render the publications page.** Due to the way the code is written this must be done locally to generate the files first before pushing changes.

7.  **Commit your code**. This will push the changes and update the website with any new publications you've added.
