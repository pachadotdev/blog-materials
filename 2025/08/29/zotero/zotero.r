# How to import an RIS or bib-export file from Zotero (Reference Manager) and convert the data into a bibliographic
# class. Zotero is open access and its export cannot be read as easy as a Clavariate-bib export file. Some fields are
# missing, such as ID (Key in Zotero export) or CI for citations. Zotero does not extract this information and I do not
# need it.

remotes::install_github("pachadotdev/zotero")

library(zotero)

x = read_zotero("bibliography.bib")
y = read_zotero("bibliography.rdf")
z = read_zotero("bibliography.ris")

write_bib(z$bib, "export.bib")
