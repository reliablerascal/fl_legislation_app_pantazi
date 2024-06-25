# Legislator Dashboard

This repo is forked from the Jacksonville Tributary's [legislator dashboard](https://github.com/apantazi/legislator_dashboard). I'm in the process of automating, streamlining, and documenting the existing data pipeline to facilitate long-term maintainability and scalability. My new work in progress is contained within two separate repos:
* [ETL Data Pipeline](https://github.com/reliablerascal/fl-legislation-app-postgres)- this revised data pipeline stores raw data from LegiScan's API, creates views, and creates an application layer to simplify access for the Shiny app and future application development. 
* [Shiny App](https://github.com/reliablerascal/fl-legislation-app-postgres)- this revised application retrieves data from the application layer

### Intended Updates ###
As I'm streamlining the architecture, I'm also improving the usefulness of the application. Following are some intended updates:
* **Visualize voting patterns for cities as well as states** by incorporating **roll call data from Jacksonville and other cities** via Legistar and transforming it into the same data model.
* **Show variation between legislator voting habits and constituent political leanings** by incorporating district-level elections data
* Clean up and simplify user interface
* Create additional static visualizations from the same application layer





