# Transactional Replication Management Mechanism for SQL Server

Use SQL Server database tables to manage and configure Transactional Replication, and set it up easily between publishers and subscribers using stored procedures.

### Huge Credit and Thanks to:
- Kostya Fridman
- Ivo Ivanov
- Martin Nikolov
- Gergana Bivolarska

### Overview
- We have a mechanism to create a replication of a database
- The mechanism would normally be located in a Configurations DB, Replication schema
- Consists of several tables, that hold the configuration of the replication and several stored procedures to recreate the replica
- Definitions are being held in `SELECT * FROM Configuration.[Replication].Definitions AS d`

### To recreate a replica from scratch, execute the following SP's. All of which expect the parameters:
  - `@DatabaseName` - The name of the database that we want to replicate from the definitions table
  - `@SubscriberInstanceName` - The name of the subscriber to which we want to replicate, again from the definitions table
  - `EXEC Configuration.[Replication].Step1_DistributerCreateAllObjects` - outputs a script that needs to be executed on the Distributor
  - `EXEC Configuration.[Replication].Step2_ArticlesVerification` - prints out tables that are missing in the article definitions. Need to take the script from one of the columns, based on the action you want to achieve:
    - If you want the article to be included, grab the value from `IncludeArticles` column and execute it in another window
    - If you want the article not to be included, grab the value from `ExcludeArticles` column and execute it in another window
    - If you want to add just this single article to an already existing replication, grab the value from `ExecAddSingleArticle` and run it in another window (check below for a full description on how to add a new article to an already running replication)
  - `EXEC Configuration.[Replication].Step3_RunPublisherProcess` - creates the publication, adds the articles, removes from replication columns that are bigger than 1000 characters, and creates the database backup to initialize the subscriber
  - `EXEC Configuration.[Replication].Step4_SubscriberCleanObjects` - outputs a script that needs to be executed on the subscriber. Restore the DB from the backup created in the previous step and prepares it for the subscription
  - `EXEC Configuration.[Replication].Step5_PublisherStart` - starts the replica
- The replica is initialized with `immediate_sync = false`. Schema changes done on the publisher are replicated to the subscriber

### When a new table is added to the publisher and you need to add it for replication:
  - Create the table also on the subscriber
    - If the table has an IDENTITY column, its definition should have the property "NOT FOR REPLICATION"
    - If the table has FK's, don't create them
    - If the table has CK's, don't create them
    - If the table has triggers, don't create them, or disable them after creation
  - Stop the log reader job
  - Sync the data between publisher and subscriber (one can use the Import/Export tool)
  - Execute `EXEC Configuration.[Replication].Step2_ArticlesVerification`, grab the output from `ExecAddSingleArticle` column for the desired table and run it in a new window.
  - Start the log reader job
