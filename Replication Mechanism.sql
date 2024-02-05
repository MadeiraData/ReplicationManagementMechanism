
SET NUMERIC_ROUNDABORT OFF
GO
SET ANSI_PADDING, ANSI_WARNINGS, CONCAT_NULL_YIELDS_NULL, ARITHABORT, QUOTED_IDENTIFIER, ANSI_NULLS ON
GO
SET XACT_ABORT ON
GO
SET TRANSACTION ISOLATION LEVEL Serializable
GO
BEGIN TRANSACTION
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating schemas'
GO
CREATE SCHEMA [Replication]
AUTHORIZATION [dbo]
GO
CREATE SCHEMA [DR]
AUTHORIZATION [dbo]
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[AdditionalObjectsPerSubscriberTypes]'
GO
CREATE TABLE [Replication].[AdditionalObjectsPerSubscriberTypes]
(
[ObjectType] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[ObjectTypeDescription] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL
)
WITH
(
DATA_COMPRESSION = PAGE
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK_AdditionalObjectsPerSubscriberTypes] on [Replication].[AdditionalObjectsPerSubscriberTypes]'
GO
ALTER TABLE [Replication].[AdditionalObjectsPerSubscriberTypes] ADD CONSTRAINT [PK_AdditionalObjectsPerSubscriberTypes] PRIMARY KEY CLUSTERED ([ObjectType]) WITH (DATA_COMPRESSION = PAGE)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[AdditionalObjectsPerSubscriber]'
GO
CREATE TABLE [Replication].[AdditionalObjectsPerSubscriber]
(
[DatabaseName] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[SchemaName] [varchar] (128) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[TabeleName] [varchar] (128) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[ObjectName] [varchar] (128) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[ObjectType] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[ObjectCreationSQL] [nvarchar] (max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[Remarks] [nvarchar] (max) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[AddedDateTime] [datetime] NULL
)
WITH
(
DATA_COMPRESSION = PAGE
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK_AdditionalObjectsPerSubscriber] on [Replication].[AdditionalObjectsPerSubscriber]'
GO
ALTER TABLE [Replication].[AdditionalObjectsPerSubscriber] ADD CONSTRAINT [PK_AdditionalObjectsPerSubscriber] PRIMARY KEY CLUSTERED ([DatabaseName], [SchemaName], [ObjectName]) WITH (DATA_COMPRESSION = PAGE)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[Databases]'
GO
CREATE TABLE [Replication].[Databases]
(
[DatabaseName] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
)
WITH
(
DATA_COMPRESSION = PAGE
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK_Databases] on [Replication].[Databases]'
GO
ALTER TABLE [Replication].[Databases] ADD CONSTRAINT [PK_Databases] PRIMARY KEY CLUSTERED ([DatabaseName]) WITH (DATA_COMPRESSION = PAGE)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[ExcludedArticleColumns]'
GO
CREATE TABLE [Replication].[ExcludedArticleColumns]
(
[DatabaseName] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[SchemaName] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[TableName] [varchar] (256) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[ColumnName] [varchar] (256) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[IsDynamicalyAdded] [bit] NOT NULL
)
WITH
(
DATA_COMPRESSION = PAGE
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK_ExcludedArticleColumns] on [Replication].[ExcludedArticleColumns]'
GO
ALTER TABLE [Replication].[ExcludedArticleColumns] ADD CONSTRAINT [PK_ExcludedArticleColumns] PRIMARY KEY CLUSTERED ([DatabaseName], [SchemaName], [TableName], [ColumnName]) WITH (DATA_COMPRESSION = PAGE)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[ExcludedArticleTables]'
GO
CREATE TABLE [Replication].[ExcludedArticleTables]
(
[DatabaseName] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[SchemaName] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[TableName] [varchar] (256) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[SubscriberInstanceName] [varchar] (256) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK_ExcludedArticleTables] on [Replication].[ExcludedArticleTables]'
GO
ALTER TABLE [Replication].[ExcludedArticleTables] ADD CONSTRAINT [PK_ExcludedArticleTables] PRIMARY KEY CLUSTERED ([DatabaseName], [SchemaName], [TableName], [SubscriberInstanceName])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[Articles]'
GO
CREATE TABLE [Replication].[Articles]
(
[DatabaseName] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[SchemaName] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[TableName] [varchar] (256) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[SubscriberInstanceName] [varchar] (256) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL CONSTRAINT [DF_ArticlesSubscriberInstanceName_ROF] DEFAULT ('PRODAMSSQLROF\ROF')
)
WITH
(
DATA_COMPRESSION = PAGE
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK_Articles] on [Replication].[Articles]'
GO
ALTER TABLE [Replication].[Articles] ADD CONSTRAINT [PK_Articles] PRIMARY KEY CLUSTERED ([DatabaseName], [SchemaName], [TableName], [SubscriberInstanceName]) WITH (DATA_COMPRESSION = PAGE)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[Definitions]'
GO
CREATE TABLE [Replication].[Definitions]
(
[DatabaseName] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[PublisherInstanceName] [varchar] (256) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[SubscriberInstanceName] [varchar] (256) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[DistributerInstanceName] [varchar] (256) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[WindowsLoginName] [varchar] (256) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[PublisherInstanceBackupFolder] [nvarchar] (1000) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[SubscriberInstanceBackupFolder] [nvarchar] (1000) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[DistributorPassword] [nvarchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL CONSTRAINT [DF_Distributor_Password] DEFAULT (right(newid(),(12))),
[WindowsLoginPassword] [nvarchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[DistributorMDFFileLocationFolder] [nvarchar] (1000) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[DistributorLDFFileLocationFolder] [nvarchar] (1000) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[DistributorWorkingDirectory] [nvarchar] (1000) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[PublisherProcessed] [bit] NOT NULL,
[IsEnabled] [bit] NOT NULL,
[IsForNY] [bit] NOT NULL,
[SubscriberMDFFileLocationFolder] [nvarchar] (1000) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[SubscriberLDFFileLocationFolder] [nvarchar] (1000) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
[IsUnLimited] [bit] NOT NULL CONSTRAINT [DF__Definitio__IsUnL__2DF1BF10] DEFAULT ((0))
)
WITH
(
DATA_COMPRESSION = PAGE
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK_Definitions] on [Replication].[Definitions]'
GO
ALTER TABLE [Replication].[Definitions] ADD CONSTRAINT [PK_Definitions] PRIMARY KEY CLUSTERED ([DatabaseName], [SubscriberInstanceName]) WITH (DATA_COMPRESSION = PAGE)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[GetBackupFileAddressOnPublisher]'
GO
/******************************************************************************************\ 
DB Object	 : Function 
Objec Name	 : [Replication].[GetBackupFileAddressOnPublisher]
Purpose		 : Build the name of the publication
Author		 : Kostya Fridman
Creation Date: 18 Nov 2014
-------------------------------------------------------------------------------------------
Demo Call:
	SELECT [Replication].[GetBackupFileAddressOnPublisher]( N'TradeNetworks', N'ProdAmsRODB\ROF' )

-------------------------------------------------------------------------------------------
Changes History:
================
Author		: Martin Nikolov
Date		: 08 Jul 2015
Description	: added additional input parameter @SubscriberInstanceName
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 30 Jul 2015
Description	: added IsEnabled check
-------------------------------------------------------------------------------------------
Author		: 
Date		: 
Description	: 
\******************************************************************************************/
CREATE   FUNCTION [Replication].[GetBackupFileAddressOnPublisher] (
    @DatabaseName           NVARCHAR (256)
  , @SubscriberInstanceName NVARCHAR (256) -- Added by M.N. 08 Jul 2015
)
RETURNS NVARCHAR (1000)
WITH SCHEMABINDING
AS
BEGIN
    RETURN (
               SELECT PublisherInstanceBackupFolder + CASE WHEN RIGHT(PublisherInstanceBackupFolder, 1) = '\' THEN '' ELSE '\' END + DatabaseName + '.' + REPLACE (PublisherInstanceName, '\', '_') + '.bak'
               FROM   [Replication].Definitions
               WHERE
                      DatabaseName           = @DatabaseName
                  AND SubscriberInstanceName = @SubscriberInstanceName -- Added by M.N. 08 Jul 2015
                  AND IsEnabled              = 1 -- Added by M.N. 30 Jul 2015
           );
END;
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[GetBackupFileAddressOnSubscriber]'
GO
/******************************************************************************************\ 
DB Object	 : Function 
Objec Name	 : [Replication].[GetBackupFileAddressOnSubscriber]
Purpose		 : Build the name of the publication
Author		 : Kostya Fridman
Creation Date: 20 Nov 2014
-------------------------------------------------------------------------------------------
Demo Call:
	SELECT [Replication].[GetBackupFileAddressOnSubscriber]( N'TradeNetworks', N'ProdAmsRODB\ROF' )

-------------------------------------------------------------------------------------------
Changes History:
================
Author		: Martin Nikolov
Date		: 08 Jul 2015
Description	: added additional input parameter @SubscriberInstanceName
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 30 Jul 2015
Description	: added IsEnabled check
-------------------------------------------------------------------------------------------
Author		: 
Date		: 
Description	: 
\******************************************************************************************/
CREATE   FUNCTION [Replication].[GetBackupFileAddressOnSubscriber] (
    @DatabaseName           NVARCHAR (256)
  , @SubscriberInstanceName NVARCHAR (256) -- Added by M.N. 08 Jul 2015
)
RETURNS NVARCHAR (1000)
WITH SCHEMABINDING
AS
BEGIN
    RETURN (
               SELECT SubscriberInstanceBackupFolder + CASE WHEN RIGHT(SubscriberInstanceBackupFolder, 1) = '\' THEN '' ELSE '\' END + DatabaseName + '.' + REPLACE (PublisherInstanceName, '\', '_') + '.bak'
               FROM   [Replication].Definitions
               WHERE
                      DatabaseName           = @DatabaseName
                  AND SubscriberInstanceName = @SubscriberInstanceName -- Added by M.N. 08 Jul 2015
                  AND IsEnabled              = 1 -- Added by M.N. 30 Jul 2015
           );
END;
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[02.PublisherAllowReplication]'
GO
/******************************************************************************************\ 
DB Object		: Stored Procedure
Objec Name		: [Replication].[02.PublisherAllowReplication]
Purpose			: Second step of the transactional replication creation
Author			: Kostya Fridman
Creation Date	: 18 Nov 2014
-------------------------------------------------------------------------------------------
Demo Call:
	sp_removedbreplication 'tradenetworks'

	EXEC [Replication].[02.PublisherAllowReplication] @DatabaseName = N'TradeNetworks', @SubscriberInstanceName = N'SOFDBA\DBA'

-------------------------------------------------------------------------------------------
Changes History:
================
Author		: Ivo Ivanov 
Date		: 19 Nov 2014
Description	: Remove hard-coded data for distributor name and password and moves it to 
				[Replication].[Definitions] table
-------------------------------------------------------------------------------------------
Author		: Kostya Fridman 
Date		: 19 Nov 2014
Description	: Remarks were added 
-------------------------------------------------------------------------------------------
Author		: Kostya Fridman 
Date		: 20 Nov 2014
Description	: run [master].sys.sp_adddistributor only when missing 
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 08 Jul 2015
Description	: added additional input parameter @SubscriberInstanceName
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 30 Jul 2015
Description	: added IsEnabled check
-------------------------------------------------------------------------------------------
Author		: 
Date		: 
Description	: 
\******************************************************************************************/
CREATE PROCEDURE [Replication].[02.PublisherAllowReplication]
    @DatabaseName           NVARCHAR (256)
  , @SubscriberInstanceName NVARCHAR (256) -- Added by M.N. 08 Jul 2015
AS
SET NOCOUNT ON;

BEGIN TRY

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Server validation
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    IF NOT EXISTS (
                      SELECT TOP 1
                             1
                      FROM   [Replication].Definitions
                      WHERE
                             DatabaseName           = @DatabaseName
                         AND SubscriberInstanceName = @SubscriberInstanceName -- Added by M.N. 08 Jul 2015
                         AND IsEnabled              = 1 -- Added by M.N. 30 Jul 2015
                         AND @@SERVERNAME           = PublisherInstanceName
                  )
    BEGIN
        RAISERROR ('Warning! You are trying to execute SP on wrong server, or with wrong combination DatabaseName/SubscriberInstanceName!
      Please check [PublisherInstanceName], [DatabaseName], [SubscriberInstanceName], [IsEnabled] in [Replication].[Definitions]!', 16, 1);
        RETURN -1;
    END;

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Get configuration parameters
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    DECLARE
        @DistributorName     VARCHAR (256)
      , @DistributorPassword VARCHAR (256);

    SELECT
         @DistributorName     = DistributerInstanceName
       , @DistributorPassword = DistributorPassword
    FROM [Replication].Definitions
    WHERE
         DatabaseName           = @DatabaseName
     AND SubscriberInstanceName = @SubscriberInstanceName; -- Added by M.N. 08 Jul 2015

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Get Distributor name and password and add it relate it to the Publisher
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

    CREATE TABLE #Distributors (
        Col1      INT
      , SrverName NVARCHAR (200)
      , col3      INT
      , col4      INT
      , col5      INT
    );
    INSERT INTO #Distributors (
                                  Col1
                                , SrverName
                                , col3
                                , col4
                                , col5
                              )
    EXEC sys.sp_get_distributor;

    IF NOT EXISTS (
                      SELECT TOP 1
                             1
                      FROM   #Distributors
                      WHERE  SrverName = @DistributorName
                  )
    BEGIN

        -- Debug
        SELECT N'debug: run exec sp_adddistributor @distributor = ''' + @DistributorName + ''', @password = ''' + @DistributorPassword + '''';

        EXEC master.sys.sp_adddistributor
            @distributor = @DistributorName
          , @password = @DistributorPassword;
    END;

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Set replication database option to publish
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

    -- Debug
    SELECT N'debug: run exec [master].sys.sp_replicationdboption @dbname = ''' + @DatabaseName + ''' , @optname = N''publish'' , @value = N''true''';

    EXEC master.sys.sp_replicationdboption
        @dbname = @DatabaseName
      , @optname = N'publish'
      , @value = N'true';

END TRY
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Catch Error:
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
BEGIN CATCH
    IF @@TRANCOUNT > 0
        ROLLBACK TRANSACTION;

    DECLARE @ErrorStr NVARCHAR (4000);
    SELECT @ErrorStr = dbo.FlushErrorString_FN ();
    RAISERROR (@ErrorStr, 16, 1);
    RETURN -1;
END CATCH;
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[CreateAdditionalObjectsPerSubscriber]'
GO
/******************************************************************************************\ 
DB Object		: Stored Procedure
Objec Name		: [Replication].[CreateAdditionalObjectsPerSubscriber]
Purpose			: Create additional objects (Indexes, Statistics, Triggers etc.) for specific
					resplication subscriber.
Author			: Martin Nikolov
Creation Date	: 14 Jul 2015
-------------------------------------------------------------------------------------------
Demo Call:
	EXEC [Replication].[CreateAdditionalObjectsPerSubscriber]
		  @DatabaseName = N'TradeNetworks'

-------------------------------------------------------------------------------------------
Changes History:
================
Author		: 
Date		: 
Description	: 
-------------------------------------------------------------------------------------------
\******************************************************************************************/
CREATE PROCEDURE [Replication].[CreateAdditionalObjectsPerSubscriber] (
    @DatabaseName NVARCHAR (256)
)
AS
SET NOCOUNT ON;

BEGIN TRY

    DECLARE @SQL NVARCHAR (MAX);

    DECLARE CursorAdditionalObjects CURSOR LOCAL STATIC READ_ONLY FORWARD_ONLY FOR
    SELECT    'USE ' + QUOTENAME(@DatabaseName) + CASE WHEN O.ObjectType NOT IN (
                                                                                'IX', 'ST'
                                                                            )
                                                   THEN '
	IF EXISTS (	SELECT TOP 1 1
				FROM sys.objects
				WHERE
					name = ''' + O.ObjectName + '''
					AND schema_id = SCHEMA_ID(''' + O.SchemaName + ''') )
		PRINT ''' + OT.ObjectTypeDescription + ' with name [' + O.ObjectName + '] in Schema [' + O.SchemaName + ']' + IIF(O.TabeleName IS NOT NULL, ' Table [' + O.TabeleName + ']', '') + ' already exists! NOTHING NEW TO CREATE!''
	ELSE'
                                                   WHEN O.ObjectType = 'IX'
                                                   THEN '
	IF EXISTS (	SELECT TOP 1 1
				FROM sys.indexes i
					INNER JOIN sys.objects o ON i.object_id = o.object_id
				WHERE
					i.type = 2
					AND i.name = ''' + O.ObjectName + '''
					AND o.is_ms_shipped = 0
					AND o.name = ''' + O.TabeleName + '''
					AND o.schema_id = SCHEMA_ID(''' + O.SchemaName + ''') )
		PRINT ''' + OT.ObjectTypeDescription + ' with name [' + O.ObjectName + '] in Schema [' + O.SchemaName + ']' + IIF(O.TabeleName IS NOT NULL, ' Table [' + O.TabeleName + ']', '') + ' already exists! NOTHING NEW TO CREATE!''
	ELSE'
                                                   WHEN O.ObjectType = 'ST'
                                                   THEN '
	IF EXISTS (	SELECT TOP 1 1
				FROM sys.stats s
					INNER JOIN sys.objects o ON s.object_id = o.object_id
				WHERE
					s.auto_created = 0
					AND s.name = ''' + O.ObjectName + '''
					AND o.is_ms_shipped = 0
					AND o.name = ''' + O.TabeleName + '''
					AND o.schema_id = SCHEMA_ID(''' + O.SchemaName + ''') )
		PRINT ''' + OT.ObjectTypeDescription + ' with name [' + O.ObjectName + '] in Schema [' + O.SchemaName + ']' + IIF(O.TabeleName IS NOT NULL, ' Table [' + O.TabeleName + ']', '') + ' already exists! NOTHING NEW TO CREATE!''
	ELSE'
                                              END + '
	BEGIN
		BEGIN TRY
			EXEC (''' + REPLACE (O.ObjectCreationSQL, '''', '''''') + ''')
			PRINT ''' + OT.ObjectTypeDescription + ' with name [' + O.ObjectName + '] in Schema [' + O.SchemaName + ']' + IIF(O.TabeleName IS NOT NULL, ' Table [' + O.TabeleName + ']', '') + ' is created successfuly!''
		END TRY
		BEGIN CATCH
			IF @@TRANCOUNT > 0
				ROLLBACK TRANSACTION;

			DECLARE	@ErrorStr NVARCHAR(4000);
			SELECT
				@ErrorStr = dbo.FlushErrorString_FN();
			RAISERROR(@ErrorStr, 16, 1);
		END CATCH
	END'
    FROM
              [Replication].AdditionalObjectsPerSubscriber      AS O
   INNER JOIN [Replication].AdditionalObjectsPerSubscriberTypes AS OT
           ON O.ObjectType = OT.ObjectType
    WHERE     O.DatabaseName = @DatabaseName;

    OPEN CursorAdditionalObjects;

    FETCH NEXT FROM CursorAdditionalObjects
    INTO @SQL;

    WHILE @@FETCH_STATUS = 0
    BEGIN
        EXEC sys.sp_executesql @SQL;

        FETCH NEXT FROM CursorAdditionalObjects
        INTO @SQL;
    END;

    CLOSE CursorAdditionalObjects;
    DEALLOCATE CursorAdditionalObjects;

    PRINT 'All Done!!!';

END TRY
BEGIN CATCH
    IF @@TRANCOUNT > 0
        ROLLBACK TRANSACTION;

    IF EXISTS (
                  SELECT TOP 1
                         1
                  FROM   sys.syscursors
                  WHERE  cursor_name = 'CursorAdditionalObjects'
              )
        DEALLOCATE CursorAdditionalObjects;

    DECLARE @ErrorStr NVARCHAR (4000);
    SELECT @ErrorStr = dbo.FlushErrorString_FN ();
    RAISERROR (@ErrorStr, 16, 1);
    RETURN -1;
END CATCH;
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[GetPublicationName]'
GO
/******************************************************************************************\ 
DB Object	 : Function 
Objec Name	 : [Replication].[GetPublicationName]
Purpose		 : Build the name of the publication
Author		 : Kostya Fridman
Creation Date: 18 Nov 2014
-------------------------------------------------------------------------------------------
Demo Call:
	SELECT [Replication].GetPublicationName( N'TradeNetworks', N'ProdAmsRODB\ROF' )

-------------------------------------------------------------------------------------------
Changes History:
================
Author		: Martin Nikolov
Date		: 08 Jul 2015
Description	: added additional input parameter @SubscriberInstanceName
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 30 Jul 2015
Description	: added IsEnabled check
-------------------------------------------------------------------------------------------
Author		: 
Date		: 
Description	: 
\******************************************************************************************/
CREATE   FUNCTION [Replication].[GetPublicationName] (
    @DatabaseName           NVARCHAR (256)
  , @SubscriberInstanceName NVARCHAR (256) -- Added by M.N. 08 Jul 2015
)
RETURNS VARCHAR (1000)
WITH SCHEMABINDING
AS
BEGIN
    RETURN (REPLACE ((
                         SELECT DatabaseName + '_' + PublisherInstanceName + '_' + SubscriberInstanceName
                         FROM   [Replication].Definitions
                         WHERE
                                DatabaseName           = @DatabaseName
                            AND SubscriberInstanceName = @SubscriberInstanceName -- Added by M.N. 08 Jul 2015
                            AND IsEnabled              = 1 -- Added by M.N. 30 Jul 2015
                     ), '\', '_'
                    )
           );
END;
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[03.PublisherAddPublication]'
GO
/******************************************************************************************\ 
DB Object		: Stored Procedure
Objec Name		: [Replication].[03.PublisherAddPublication]
Purpose			: Third step of the transactional replication creation
Author			: Kostya Fridman
Creation Date	: 18 Nov 2014
-------------------------------------------------------------------------------------------
Demo Call:
	EXEC [Replication].[03.PublisherAddPublication] @DatabaseName = N'TradeNetworks', @SubscriberInstanceName = N'ProdAmsRODB\ROF'

-------------------------------------------------------------------------------------------
Changes History:
================
Author		: Ivo Ivanov
Date		: 19 Nov 2014
Description	: Add some remarks
-------------------------------------------------------------------------------------------
Author		: Kostya Fridman 
Date		: 19 Nov 2014
Description	: Remarks were added 
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 08 Jul 2015
Description	: added additional input parameter @SubscriberInstanceName
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 30 Jul 2015
Description	: added IsEnabled check
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 02 Nov 2021
Description	: Added allow_partition_switch = 'true'
-------------------------------------------------------------------------------------------
Author		: 
Date		: 
Description	: 
\******************************************************************************************/
CREATE PROCEDURE [Replication].[03.PublisherAddPublication]
    @DatabaseName           NVARCHAR (256)
  , @SubscriberInstanceName NVARCHAR (256) -- Added by M.N. 08 Jul 2015
AS
SET NOCOUNT ON;

BEGIN TRY

    IF NOT EXISTS (
                      SELECT TOP 1
                             1
                      FROM   [Replication].Definitions
                      WHERE
                             DatabaseName           = @DatabaseName
                         AND SubscriberInstanceName = @SubscriberInstanceName -- Added by M.N. 08 Jul 2015
                         AND IsEnabled              = 1 -- Added by M.N. 30 Jul 2015
                         AND @@SERVERNAME           = PublisherInstanceName
                  )
    BEGIN
        RAISERROR ('Warning! You are trying to execute SP on wrong server, or with wrong combination DatabaseName/SubscriberInstanceName!
      Please check [PublisherInstanceName], [DatabaseName], [SubscriberInstanceName], [IsEnabled] in [Replication].[Definitions]!', 16, 1);
        RETURN -1;
    END;

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Declaration of variables
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    DECLARE
        @publication                  sysname -- 256
      , @taskid                       INT     -- 4
      , @restricted                   NVARCHAR (10)
      , @sync_method                  NVARCHAR (40)
      , @repl_freq                    NVARCHAR (10)
      , @description                  NVARCHAR (255)
      , @status                       NVARCHAR (8)
      , @independent_agent            NVARCHAR (5)
      , @immediate_sync               NVARCHAR (5)
      , @enabled_for_internet         NVARCHAR (5)
      , @allow_push                   NVARCHAR (5)
      , @allow_pull                   NVARCHAR (5)
      , @allow_anonymous              NVARCHAR (5)
      , @allow_sync_tran              NVARCHAR (5)
      , @autogen_sync_procs           NVARCHAR (5)
      , @retention                    INT     -- 4
      , @allow_queued_tran            NVARCHAR (5)
      , @snapshot_in_defaultfolder    NVARCHAR (5)
      , @alt_snapshot_folder          NVARCHAR (255)
      , @pre_snapshot_script          NVARCHAR (255)
      , @post_snapshot_script         NVARCHAR (255)
      , @compress_snapshot            NVARCHAR (5)
      , @ftp_address                  sysname -- 256
      , @ftp_port                     INT     -- 4
      , @ftp_subdirectory             NVARCHAR (255)
      , @ftp_login                    sysname -- 256
      , @ftp_password                 sysname -- 256
      , @allow_dts                    NVARCHAR (5)
      , @allow_subscription_copy      NVARCHAR (5)
      , @conflict_policy              NVARCHAR (100)
      , @centralized_conflicts        NVARCHAR (5)
      , @conflict_retention           INT     -- 4
      , @queue_type                   NVARCHAR (10)
      , @add_to_active_directory      NVARCHAR (10)
      , @logreader_job_name           sysname -- 256
      , @qreader_job_name             sysname -- 256
      , @publisher                    sysname -- 256
      , @allow_initialize_from_backup NVARCHAR (5)
      , @replicate_ddl                INT     -- 4
      , @enabled_for_p2p              NVARCHAR (5)
      , @publish_local_changes_only   NVARCHAR (5)
      , @enabled_for_het_sub          NVARCHAR (5)
      , @p2p_conflictdetection        NVARCHAR (5)
      , @p2p_originator_id            INT     -- 4
      , @p2p_continue_onconflict      NVARCHAR (5)
      , @allow_partition_switch       NVARCHAR (5)
      , @replicate_partition_switch   NVARCHAR (5);

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Set the values of the variables
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    SELECT
        @publication                  = [Replication].GetPublicationName (@DatabaseName, @SubscriberInstanceName) -- Changed by M.N. 08 Jul 2015, added @SubscriberInstanceName
      , @description                  = N'Transactional publication of database ' + QUOTENAME(@DatabaseName) + N' from Publisher ' + (
                                                                                                                                SELECT PublisherInstanceName
                                                                                                                                FROM   [Replication].Definitions
                                                                                                                                WHERE
                                                                                                                                       DatabaseName           = @DatabaseName
                                                                                                                                   AND SubscriberInstanceName = @SubscriberInstanceName -- Added by M.N. 08 Jul 2015
                                                                                                                            ) + N' to Subscriber ' + (
                                                                                                                                                         SELECT SubscriberInstanceName
                                                                                                                                                         FROM   [Replication].Definitions
                                                                                                                                                         WHERE
                                                                                                                                                                DatabaseName           = @DatabaseName
                                                                                                                                                            AND SubscriberInstanceName = @SubscriberInstanceName -- Added by M.N. 08 Jul 2015
                                                                                                                                                     )
      , @sync_method                  = N'concurrent'
      , @retention                    = 0
      , @allow_push                   = N'true'
      , @allow_pull                   = N'true'
      , @allow_anonymous              = N'false'
      , @enabled_for_internet         = N'false'
      , @snapshot_in_defaultfolder    = N'true'
      , @compress_snapshot            = N'false'
      , @ftp_port                     = 21
      , @allow_subscription_copy      = N'false'
      , @add_to_active_directory      = N'false'
      , @repl_freq                    = N'continuous'
      , @status                       = N'active'
      , @independent_agent            = N'true'
      , @immediate_sync               = N'false'                                                                  ----!
      , @allow_sync_tran              = N'false'
      , @allow_queued_tran            = N'false'
      , @allow_dts                    = N'false'
      , @replicate_ddl                = 1
      , @allow_initialize_from_backup = N'true'                                                                   ----!
      , @enabled_for_p2p              = N'false'
      , @enabled_for_het_sub          = N'false'
                                                                                                                  --			, @taskid						-- Supported for backward compatibility only;
                                                                                                                  --			, @restricted					-- Supported for backward compatibility only;
                                                                                                                  --			, @autogen_sync_procs			-- NULL (default) 
                                                                                                                  --			, @alt_snapshot_folder			-- NULL (default)
                                                                                                                  --			, @pre_snapshot_script			-- NULL (default)
                                                                                                                  --			, @post_snapshot_script			-- NULL (default)
                                                                                                                  --			, @ftp_address					-- NULL (default)
                                                                                                                  --			, @ftp_subdirectory				-- NULL (default)
                                                                                                                  --			, @ftp_login					-- default of ANONYMOUS
                                                                                                                  --			, @ftp_password					-- NULL (default)
                                                                                                                  --			, @conflict_policy				-- NULL (default)
                                                                                                                  --			, @centralized_conflicts		-- NULL (default)
                                                                                                                  --			, @conflict_retention			-- 14 ( default):  Specifies the conflict retention period, in days.
                                                                                                                  --			, @queue_type					-- NULL (default)
                                                                                                                  --			, @logreader_job_name			-- NULL (default)
                                                                                                                  --			, @qreader_job_name				-- NULL (default)
                                                                                                                  --			, @publisher					-- NULL (default)
                                                                                                                  --			, @publish_local_changes_only	-- NULL: Identified for informational purposes only. Not supported
                                                                                                                  --			, @p2p_conflictdetection		-- default value of TRUE
                                                                                                                  --			, @p2p_originator_id			-- NULL (default)
                                                                                                                  --			, @p2p_continue_onconflict		-- NULL (default)
      , @allow_partition_switch       = N'true'
      , @replicate_partition_switch   = N'false';                                                                 -- NULL (default)

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Prepare and execute a dynamic sql to add publication to the Publisher machine
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    DECLARE @SQL NVARCHAR (MAX) = N'EXEC ' + QUOTENAME(@DatabaseName) + N'.[sys].sp_addpublication 
			  @publication						= @publication     
--			, @taskid							= @taskid       
--			, @restricted						= @restricted      
			, @sync_method						= @sync_method     
			, @repl_freq						= @repl_freq      
			, @description						= @description     
			, @status							= @status       
			, @independent_agent				= @independent_agent    
			, @immediate_sync					= @immediate_sync     
			, @enabled_for_internet				= @enabled_for_internet   
			, @allow_push						= @allow_push      
			, @allow_pull						= @allow_pull      
			, @allow_anonymous					= @allow_anonymous    
			, @allow_sync_tran					= @allow_sync_tran    
			, @autogen_sync_procs				= @autogen_sync_procs    
			, @retention						= @retention      
			, @allow_queued_tran				= @allow_queued_tran    
			, @snapshot_in_defaultfolder		= @snapshot_in_defaultfolder  
			, @alt_snapshot_folder				= @alt_snapshot_folder   
			, @pre_snapshot_script				= @pre_snapshot_script   
			, @post_snapshot_script				= @post_snapshot_script   
			, @compress_snapshot				= @compress_snapshot    
			, @ftp_address						= @ftp_address     
			, @ftp_port							= @ftp_port      
			, @ftp_subdirectory					= @ftp_subdirectory    
--			, @ftp_login						= @ftp_login      
			, @ftp_password						= @ftp_password     
			, @allow_dts						= @allow_dts      
			, @allow_subscription_copy			= @allow_subscription_copy  
			, @conflict_policy					= @conflict_policy    
			, @centralized_conflicts			= @centralized_conflicts   
--			, @conflict_retention				= @conflict_retention    
			, @queue_type						= @queue_type      
			, @add_to_active_directory			= @add_to_active_directory  
			, @logreader_job_name				= @logreader_job_name    
			, @qreader_job_name					= @qreader_job_name    
			, @publisher						= @publisher      
			, @allow_initialize_from_backup		= @allow_initialize_from_backup 
			, @replicate_ddl					= @replicate_ddl     
			, @enabled_for_p2p					= @enabled_for_p2p    
--			, @publish_local_changes_only		= @publish_local_changes_only  
			, @enabled_for_het_sub				= @enabled_for_het_sub   
--			, @p2p_conflictdetection			= @p2p_conflictdetection   
			, @p2p_originator_id				= @p2p_originator_id    
			, @p2p_continue_onconflict			= @p2p_continue_onconflict  
			, @allow_partition_switch			= @allow_partition_switch   
			, @replicate_partition_switch		= @replicate_partition_switch  
    ';
    -- Debug 
    PRINT @SQL;

    EXEC sys.sp_executesql
        @SQL
      , N'
				  @publication						sysname  -- 256
--				, @taskid							int   -- 4
--				, @restricted						nvarchar ( 10 )
				, @sync_method						nvarchar ( 40 )
				, @repl_freq						nvarchar ( 10 )
				, @description						nvarchar ( 255  )
				, @status							nvarchar ( 8 )
				, @independent_agent				nvarchar ( 5 )
				, @immediate_sync					nvarchar ( 5 )
				, @enabled_for_internet				nvarchar ( 5 )
				, @allow_push						nvarchar ( 5 )
				, @allow_pull						nvarchar ( 5 )
				, @allow_anonymous					nvarchar ( 5 )
				, @allow_sync_tran					nvarchar ( 5 )
				, @autogen_sync_procs				nvarchar ( 5 )
				, @retention						int   -- 4
				, @allow_queued_tran				nvarchar ( 5 )
				, @snapshot_in_defaultfolder		nvarchar ( 5 )
				, @alt_snapshot_folder				nvarchar ( 255 )
				, @pre_snapshot_script				nvarchar ( 255 )
				, @post_snapshot_script				nvarchar ( 255 )
				, @compress_snapshot				nvarchar ( 5 )
				, @ftp_address						sysname  -- 256
				, @ftp_port							int   -- 4
				, @ftp_subdirectory					nvarchar ( 255 )
--				, @ftp_login						sysname  -- 256
				, @ftp_password						sysname  -- 256
				, @allow_dts						nvarchar ( 5 )
				, @allow_subscription_copy			nvarchar ( 5 )
				, @conflict_policy					nvarchar ( 100 )
				, @centralized_conflicts			nvarchar ( 5 )
--				, @conflict_retention				int   -- 4
				, @queue_type						nvarchar ( 10 )
				, @add_to_active_directory			nvarchar ( 10 )
				, @logreader_job_name				sysname  -- 256
				, @qreader_job_name					sysname  -- 256
				, @publisher						sysname  -- 256
				, @allow_initialize_from_backup		nvarchar ( 5 )
				, @replicate_ddl					int   -- 4
				, @enabled_for_p2p					nvarchar ( 5 )
--				, @publish_local_changes_only		nvarchar ( 5 )
				, @enabled_for_het_sub				nvarchar ( 5 )
--				, @p2p_conflictdetection			nvarchar ( 5 )
				, @p2p_originator_id				int   -- 4
				, @p2p_continue_onconflict			nvarchar ( 5 )
				, @allow_partition_switch			nvarchar ( 5 )
				, @replicate_partition_switch		nvarchar ( 5 )
		'
      , @publication = @publication
      --		, @taskid									= @taskid       
      --		, @restricted								= @restricted      
      , @sync_method = @sync_method
      , @repl_freq = @repl_freq
      , @description = @description
      , @status = @status
      , @independent_agent = @independent_agent
      , @immediate_sync = @immediate_sync
      , @enabled_for_internet = @enabled_for_internet
      , @allow_push = @allow_push
      , @allow_pull = @allow_pull
      , @allow_anonymous = @allow_anonymous
      , @allow_sync_tran = @allow_sync_tran
      , @autogen_sync_procs = @autogen_sync_procs
      , @retention = @retention
      , @allow_queued_tran = @allow_queued_tran
      , @snapshot_in_defaultfolder = @snapshot_in_defaultfolder
      , @alt_snapshot_folder = @alt_snapshot_folder
      , @pre_snapshot_script = @pre_snapshot_script
      , @post_snapshot_script = @post_snapshot_script
      , @compress_snapshot = @compress_snapshot
      , @ftp_address = @ftp_address
      , @ftp_port = @ftp_port
      , @ftp_subdirectory = @ftp_subdirectory
      --		, @ftp_login								= @ftp_login      
      , @ftp_password = @ftp_password
      , @allow_dts = @allow_dts
      , @allow_subscription_copy = @allow_subscription_copy
      , @conflict_policy = @conflict_policy
      , @centralized_conflicts = @centralized_conflicts
      --		, @conflict_retention						= @conflict_retention    
      , @queue_type = @queue_type
      , @add_to_active_directory = @add_to_active_directory
      , @logreader_job_name = @logreader_job_name
      , @qreader_job_name = @qreader_job_name
      , @publisher = @publisher
      , @allow_initialize_from_backup = @allow_initialize_from_backup
      , @replicate_ddl = @replicate_ddl
      , @enabled_for_p2p = @enabled_for_p2p
      --		, @publish_local_changes_only				= @publish_local_changes_only  
      , @enabled_for_het_sub = @enabled_for_het_sub
      --		, @p2p_conflictdetection					= @p2p_conflictdetection   
      , @p2p_originator_id = @p2p_originator_id
      , @p2p_continue_onconflict = @p2p_continue_onconflict
      , @allow_partition_switch = @allow_partition_switch
      , @replicate_partition_switch = @replicate_partition_switch;

END TRY
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Catch Error:
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
BEGIN CATCH
    IF @@TRANCOUNT > 0
        ROLLBACK TRANSACTION;

    DECLARE @ErrorStr NVARCHAR (4000);
    SELECT @ErrorStr = dbo.FlushErrorString_FN ();
    RAISERROR (@ErrorStr, 16, 1);
    RETURN -1;
END CATCH;
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[EnableDisableJobsOnDistributer]'
GO
/******************************************************************************************\
DB Object	 : Stored Procedure
Objec Name	 : [Replication].[EnableDisableJobsOnDistributer]
Purpose		 : To Enable/Disable all jobs on Distributer 
Author		 : Martin Nikolov
Creation Date: 28 Nov 2014
-------------------------------------------------------------------------------------------
Demo Call:
   -- Important: It should be run on the Distributor machine!
	EXEC [Replication].[EnableDisableJobsOnDistributer] @DatabaseName = N'TradeNetworks', @enabled = 0

-------------------------------------------------------------------------------------------
Changes History:
================
Author		 : Martin Nikolov
Date		 : 28 Nov 2014
Description  : Preparing a dynamic SQL code for the main part of the procedure. 
			   Checking the @@SERVERNAME at the begining.
\******************************************************************************************/
CREATE PROCEDURE [Replication].[EnableDisableJobsOnDistributer]
    @DatabaseName NVARCHAR (256)
  , @enabled      BIT
AS
SET NOCOUNT ON;

BEGIN TRY


    SELECT '
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- Run returned string on DISTRIBUTER!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
'   ;


    DECLARE @SQL NVARCHAR (MAX);
    SELECT @SQL = N'
		
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
|	Check if the procedure is executed on the Distributor machine
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

		IF (@@SERVERNAME NOT LIKE ''' + DistributerInstanceName + N''' )
		BEGIN
			RAISERROR( ''Warning! You are trying to execute script on a wrong server!
		     Please use distributor or check your Distributor`s name!'' , 16, 1)
		END
		ELSE 
		BEGIN

			USE [msdb]

			DECLARE
				  @job_name NVARCHAR(128)
				, @new_name NVARCHAR(128)
				, @description NVARCHAR(512)
				, @enabled TINYINT
				, @SQL NVARCHAR(MAX);

			CREATE TABLE #JobsList
						( Id INT IDENTITY(1,1) NOT NULL
						, [JobName] NVARCHAR(255) NOT NULL 
						, [JobNewName] NVARCHAR(255) NOT NULL 
						, [JobDesctiption] NVARCHAR(255) NOT NULL
						, [Enabled] TINYINT )
				' + CASE @enabled
                         WHEN 1
                         THEN '
			INSERT INTO #JobsList ( [JobName], [JobNewName], [JobDesctiption], [Enabled] )
			SELECT name, REPLACE(name, '' - TempDisabled'', ''''), REPLACE(description, '' Job temporaly disabled for ...'', ''''), 1
			FROM sysjobs
			WHERE   name like ''% - TempDisabled''
			'
                         WHEN 0
                         THEN '
			INSERT INTO #JobsList ( [JobName], [JobNewName], [JobDesctiption], [Enabled] )
			SELECT name,  name + '' - TempDisabled'', description + '' Job temporaly disabled for ...'', 0
			FROM sysjobs
			WHERE   name <> ''syspolicy_purge_history''
				AND name NOT LIKE ''% - TempDisabled''
				AND [enabled] = 1
			'
                    END + N'
			SELECT @SQL = ''EXEC msdb..sp_update_job @job_name = @job_name, @new_name = @new_name, @description = @description, @enabled = @enabled''

			DECLARE @Id INT = 0
			WHILE (1=1)
				BEGIN 

				SELECT    @job_name		= NULL
						, @new_name		= NULL
						, @description	= NULL
						, @enabled		= NULL

				SELECT TOP 1 
					   @Id = Id
					 , @job_name = JobName
					 , @new_name = JobNewName
					 , @description = JobDesctiption
					 , @enabled = Enabled
				FROM #JobsList
				WHERE Id > @Id
				ORDER BY Id

				IF @job_name IS NULL BREAK
	
				EXEC SP_EXECUTESQL @SQL
							,N''@job_name		nvarchar(128)
							, @new_name			nvarchar(128)
							, @description		nvarchar(512)
							, @enabled			tinyint
							''                        
							, @job_name			= @job_name
							, @new_name			= @new_name
							, @description		= @description
							, @enabled			= @enabled
			END -- End of the loop
		END
'
    FROM   [Replication].Definitions
    WHERE  DatabaseName = @DatabaseName;


    SELECT @SQL;

END TRY
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\
| Catch Error:
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
BEGIN CATCH
    IF @@TRANCOUNT > 0
        ROLLBACK TRANSACTION;

    DECLARE @ErrorStr NVARCHAR (4000);
    SELECT @ErrorStr = dbo.FlushErrorString_FN ();
    RAISERROR (@ErrorStr, 16, 1);
    RETURN -1;
END CATCH;

GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[04.PublisherAddArticles]'
GO
/******************************************************************************************\ 
DB Object		: Stored Procedure
Objec Name		: [Replication].[04.PublisherAddArticles]
Purpose			: Add articles  step of the transactional replication
Author			: Kostya Fridman
Creation Date	: 18 Nov 2014
-------------------------------------------------------------------------------------------
Demo Call:
	EXEC [Replication].[04.PublisherAddArticles] @DatabaseName = N'Configuration', @SubscriberInstanceName = N'SOFDBA\DBA'

-------------------------------------------------------------------------------------------
Changes History:
================
Author		: Martin Nikolov
Date		: 29 Jun 2015
Description	: Changed the default sp_addarticle @status parameter to 24. BOL description:
				"Specifies if the article is active and additional options for how changes are propagated. status is tinyint, and can be the | (Bitwise OR) product of one or more of these values."
				" 16 (default) - Uses parameterized statements."
				" 24 - Includes the column name in INSERT statements and uses parameterized statements."
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 08 Jul 2015
Description	: added additional input parameter @SubscriberInstanceName
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 30 Jul 2015
Description	: added IsEnabled check
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 03 May 2016
Description	: Added filter on DatabaseName when it takes articles for replication
-------------------------------------------------------------------------------------------
Author		: Gergana Bivolarska
Approved By : Martin Nikolov
Date		: 15 Jan 2019
Description	: #87526 - Different articles per subscriber
\******************************************************************************************/
CREATE PROCEDURE [Replication].[04.PublisherAddArticles]
    @DatabaseName           NVARCHAR (256)
  , @SubscriberInstanceName NVARCHAR (256) -- Added by M.N. 08 Jul 2015
AS
SET NOCOUNT ON;

BEGIN TRY

    IF NOT EXISTS (
                      SELECT TOP 1
                             1
                      FROM   [Replication].Definitions
                      WHERE
                             DatabaseName           = @DatabaseName
                         AND SubscriberInstanceName = @SubscriberInstanceName -- Added by M.N. 08 Jul 2015
                         AND IsEnabled              = 1 -- Added by M.N. 30 Jul 2015
                         AND @@SERVERNAME           = PublisherInstanceName
                  )
    BEGIN
        RAISERROR ('Warning! You are trying to execute SP on wrong server, or with wrong combination DatabaseName/SubscriberInstanceName!
      Please check [PublisherInstanceName], [DatabaseName], [SubscriberInstanceName], [IsEnabled] in [Replication].[Definitions]!', 16, 1);
        RETURN -1;
    END;

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Declare variables & temp tables
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    DECLARE
        @publication                   sysname
      , @article                       sysname
      , @source_table                  NVARCHAR (386)
      , @destination_table             sysname
      , @vertical_partition            NCHAR (5)
      , @type                          sysname
      , @filter                        NVARCHAR (386)
      , @sync_object                   NVARCHAR (386)
      , @ins_cmd                       NVARCHAR (255)
      , @del_cmd                       NVARCHAR (255)
      , @upd_cmd                       NVARCHAR (255)
      , @creation_script               NVARCHAR (255)
      , @description                   NVARCHAR (255)
      , @pre_creation_cmd              NVARCHAR (10)
                                                     --			, @filter_clause      ntext
      , @schema_option                 VARBINARY (4)
      , @destination_owner             sysname
      , @status                        TINYINT       --- Specifies if the article is active and additional options for how changes are propagated. status is tinyint, and can be the | (Bitwise OR) product of one or more of these values.                                
      , @source_owner                  sysname
      , @sync_object_owner             sysname
      , @filter_owner                  sysname
      , @source_object                 sysname
      , @artid                         INT
      , @auto_identity_range           NVARCHAR (5)
      , @pub_identity_range            BIGINT
      , @identity_range                BIGINT
      , @threshold                     INT
      , @force_invalidate_snapshot     BIT           -- Acknowledges that the action taken by this stored procedure may invalidate an existing snapshot. force_invalidate_snapshot is a bit, with a default of 0.                                
      , @use_default_datatypes         BIT           -- Is whether the default column data type mappings are used when publishing an article from an Oracle Publisher. use_default_datatypes is bit, with a default of 1.                              
      , @identityrangemanagementoption NVARCHAR (10)
      , @publisher                     sysname
      , @fire_triggers_on_snapshot     NVARCHAR (5); --Is if replicated user triggers are executed when the initial snapshot is applied. fire_triggers_on_snapshot is nvarchar(5), with a default of FALSE.   
    DECLARE
        @TableName  VARCHAR (256)
      , @SchemaName VARCHAR (256);

    -- List of constants: Defined by Shemer
    SELECT
        @publication                   = [Replication].GetPublicationName (@DatabaseName, @SubscriberInstanceName) -- Changed by M.N. 08 Jul 2015, added @SubscriberInstanceName
      , @type                          = N'logbased'
      , @description                   = NULL
      , @creation_script               = NULL
      , @pre_creation_cmd              = N'drop'
      , @schema_option                 = 0x000000000803509F
      , @identityrangemanagementoption = N'manual'
      , @vertical_partition            = N'false'
      , @ins_cmd                       = N'SQL'
      , @del_cmd                       = N'SQL'
      , @upd_cmd                       = N'SQL'
      , @status                        = 24;                                                                       -- Added by M.N. 28 Jun 2015

    CREATE TABLE #Articles (
        Id         INT           IDENTITY (1, 1) NOT NULL
      , SchemaName VARCHAR (256) NOT NULL
      , TableName  VARCHAR (256) NOT NULL
    );

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Get all articles into temp table from the relevant database. 
| Join with [Replication].[Definitions]
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

    DECLARE @SQL NVARCHAR (MAX) = N'
			SELECT c.name as SchemaName
				, b.name as TableName
--				, b.object_id
			FROM ' + QUOTENAME(@DatabaseName) + N'.sys.key_constraints a  
				INNER JOIN ' + QUOTENAME(@DatabaseName) + N'.sys.tables b ON a.parent_object_id = b.OBJECT_ID  
				INNER JOIN ' + QUOTENAME(@DatabaseName) + N'.sys.schemas c ON a.schema_id = c.schema_id
                INNER JOIN ' + QUOTENAME(DB_NAME()) + N'.[Replication].[Articles] ar on ar.[SchemaName] = c.name and ar.[TableName] = b.name AND ar.DatabaseName = ''' + @DatabaseName + N''' 
                AND ar.SubscriberInstanceName = ''' + @SubscriberInstanceName + N''' --Added by G.B. 14 Jul 2019
			WHERE
				a.type = ''PK''
				AND b.type = ''U''
				AND b.is_ms_shipped = 0
			ORDER BY 1, 2';
    -- Debug
    PRINT @SQL;
    INSERT INTO #Articles (
                              SchemaName
                            , TableName
                          )
    EXEC sys.sp_executesql @SQL;

    -- Debug
    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Add articles one by one
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    DECLARE @Id INT = 0;
    WHILE (1 = 1)
    BEGIN

        SELECT
            @TableName  = NULL
          , @SchemaName = NULL;

        SELECT   TOP 1
                 @Id         = Id
               , @SchemaName = SchemaName
               , @TableName  = TableName
        FROM     #Articles
        WHERE    Id > @Id
        ORDER BY Id;

        IF @TableName IS NULL
            BREAK;

        SELECT
            @article           = @SchemaName + '.' + @TableName
          , @source_owner      = @SchemaName
          , @source_object     = @TableName
          , @destination_table = @TableName
          , @destination_owner = @SchemaName;

        SELECT @SQL = N'EXEC ' + QUOTENAME(@DatabaseName) + N'.sys.sp_addarticle 
					  @publication						= @publication                                   
					, @article							= @article                                      
					, @source_table						= @source_table                                        
					, @destination_table				= @destination_table             
					, @vertical_partition				= @vertical_partition                                
					, @type								= @type                          
					, @filter							= @filter                                              
					, @sync_object						= @sync_object                                         
					, @ins_cmd							= @ins_cmd                                             
					, @del_cmd							= @del_cmd                                             
					, @upd_cmd							= @upd_cmd                                             
					, @creation_script					= @creation_script                                     
					, @description						= @description                                         
					, @pre_creation_cmd					= @pre_creation_cmd                                   
--					, @filter_clause					= @filter_clause                                     
					, @schema_option					= @schema_option                                     
					, @destination_owner				= @destination_owner             
					, @status							= @status                                           -- Included by M.N. 28 Jun 2015
					, @source_owner						= @source_owner                  
					, @sync_object_owner				= @sync_object_owner             
					, @filter_owner						= @filter_owner                  
					, @source_object					= @source_object                 
					, @artid							= @artid                                        
					, @auto_identity_range				= @auto_identity_range                               
					, @pub_identity_range				= @pub_identity_range            
					, @identity_range					= @identity_range                                  
					, @threshold						= @threshold                                    
--					, @force_invalidate_snapshot		= @force_invalidate_snapshot                    
--					, @use_default_datatypes			= @use_default_datatypes                        
					, @identityrangemanagementoption	= @identityrangemanagementoption                      
					, @publisher						= @publisher                     
--					, @fire_triggers_on_snapshot		= @fire_triggers_on_snapshot   
				';
        -- Print debug information
        PRINT 'Add article ' + @article;

        EXEC sys.sp_executesql
            @SQL
          , N'  @publication						sysname                                            
					, @article                          sysname                              
					, @source_table                     nvarchar  (386)                             
					, @destination_table                sysname   
					, @vertical_partition               nchar     (5)                             
					, @type                             sysname   
					, @filter                           nvarchar  (386)                             
					, @sync_object                      nvarchar  (386)                             
					, @ins_cmd                          nvarchar  (255)                             
					, @del_cmd                          nvarchar  (255)                             
					, @upd_cmd                          nvarchar  (255)                             
					, @creation_script                  nvarchar  (255)                             
					, @description                      nvarchar  (255)                             
					, @pre_creation_cmd                 nvarchar  (10)                             
--					, @filter_clause                    ntext     (8)                             
					, @schema_option                    varbinary (4)                             
					, @destination_owner                sysname   
					, @status                           tinyint                                             -- Included by M.N. 28 Jun 2015
					, @source_owner                     sysname   
					, @sync_object_owner                sysname   
					, @filter_owner                     sysname   
					, @source_object                    sysname   
					, @artid                            int                                  
					, @auto_identity_range              nvarchar  (5)                             
					, @pub_identity_range               bigint    
					, @identity_range                   bigint                                  
					, @threshold                        int                                  
--					, @force_invalidate_snapshot        bit                                  
--					, @use_default_datatypes            bit                                  
					, @identityrangemanagementoption    nvarchar  (10)                             
					, @publisher                        sysname   
--					, @fire_triggers_on_snapshot        nvarchar  (5) 
				'
          , @publication = @publication
          , @article = @article
          , @source_table = @source_table
          , @destination_table = @destination_table
          , @vertical_partition = @vertical_partition
          , @type = @type
          , @filter = @filter
          , @sync_object = @sync_object
          , @ins_cmd = @ins_cmd
          , @del_cmd = @del_cmd
          , @upd_cmd = @upd_cmd
          , @creation_script = @creation_script
          , @description = @description
          , @pre_creation_cmd = @pre_creation_cmd
                              --				, @filter_clause						= @filter_clause                                     
          , @schema_option = @schema_option
          , @destination_owner = @destination_owner
          , @status = @status -- Included by M.N. 28 Jun 2015
          , @source_owner = @source_owner
          , @sync_object_owner = @sync_object_owner
          , @filter_owner = @filter_owner
          , @source_object = @source_object
          , @artid = @artid
          , @auto_identity_range = @auto_identity_range
          , @pub_identity_range = @pub_identity_range
          , @identity_range = @identity_range
          , @threshold = @threshold
                              --				, @force_invalidate_snapshot			= @force_invalidate_snapshot
                              --				, @use_default_datatypes				= @use_default_datatypes
          , @identityrangemanagementoption = @identityrangemanagementoption
          , @publisher = @publisher;
        --				, @fire_triggers_on_snapshot			= @fire_triggers_on_snapshot

        -- Print debug information
        PRINT 'The article ' + @article + ' was added to the publication ' + @publication;

    END; -- End of the loop

END TRY
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Catch Error:
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
BEGIN CATCH
    IF @@TRANCOUNT > 0
        ROLLBACK TRANSACTION;

    DECLARE @ErrorStr NVARCHAR (4000);
    SELECT @ErrorStr = dbo.FlushErrorString_FN ();
    RAISERROR (@ErrorStr, 16, 1);
    RETURN -1;
END CATCH;
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[05.PublisherDropSpecificColumns]'
GO
/******************************************************************************************\ 
DB Object	 : Stored Procedure
Objec Name	 : [Replication].[05.PublisherDropSpecificColumns]
Purpose		 : Remove article's columns from the transactional replication
Author		 : Kostya Fridman & Ivo Ivanov
Creation Date: 18 Nov 2014
-------------------------------------------------------------------------------------------
Demo Call:

	EXEC [Replication].[05.PublisherDropSpecificColumns] @DatabaseName = N'Configuration', @SubscriberInstanceName = N'SOFQAPROD-REP'
	 
-------------------------------------------------------------------------------------------
Changes History:
================
Author		 : Martin Nikolov
Date		 : 25 Jun 2015
Description  : Removed the temp table #ArticleColumns and replaced it wit permanent one [Replication].[ExcludedArticleColumns]
				in which we can manualy define colums that we want to exclude from the replication (IsDynamicalyAdded = 0).
				Replaced the WHILE loop with CURSOR
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 08 Jul 2015
Description	: added additional input parameter @SubscriberInstanceName.
				Added filter of articles from which to drop columns by publication.
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 30 Jul 2015
Description	: added IsEnabled check
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 27 Nov 2017
Description	: Included a check for the DatabaseName on ProtectedArticleColumns
-------------------------------------------------------------------------------------------
Author		: Gergana Bivolarska 
Approved By : Martin Nikolov
Date		: 19 Dec 2018
Description	: Added isUnlimited column in [Replication].[Definitions] table for AWS 
-------------------------------------------------------------------------------------------
Author		: Gergana Bivolarska 
Approved By : Martin Nikolov
Date		: 15 Jan 2019
Description	: #87527 - Add an option for not limited replication
\******************************************************************************************/
CREATE PROCEDURE [Replication].[05.PublisherDropSpecificColumns]
    @DatabaseName           NVARCHAR (256)
  , @SubscriberInstanceName NVARCHAR (256) -- Added by M.N. 08 Jul 2015
AS
SET NOCOUNT ON;

BEGIN TRY

    IF NOT EXISTS (
                      SELECT TOP 1
                             1
                      FROM   [Replication].Definitions
                      WHERE
                             DatabaseName           = @DatabaseName
                         AND SubscriberInstanceName = @SubscriberInstanceName -- Added by M.N. 08 Jul 2015
                         AND IsEnabled              = 1 -- Added by M.N. 30 Jul 2015
                         AND @@SERVERNAME           = PublisherInstanceName
                  )
    BEGIN
        RAISERROR ('Warning! You are trying to execute SP on wrong server, or with wrong combination DatabaseName/SubscriberInstanceName!
      Please check [PublisherInstanceName], [DatabaseName], [SubscriberInstanceName], [IsEnabled] in [Replication].[Definitions]!', 16, 1);
        RETURN -1;
    END;

    --Added by Gergana Bivolarska 17 Dec 2018
    DECLARE @IsUnLimited BIT;
    SELECT @IsUnLimited = IsUnLimited
    FROM   [Replication].Definitions
    WHERE
           DatabaseName           = @DatabaseName
       AND SubscriberInstanceName = @SubscriberInstanceName
       AND IsEnabled              = 1;
    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Declare variables & temp tables
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    DECLARE
        @publication               sysname
      , @article                   sysname
      , @column                    sysname
      , @operation                 NVARCHAR (5)
      , @refresh_synctran_procs    BIT     -- Specifies whether the stored procedures supporting immediate updating subscriptions are regenerated to match the number of columns replicated. refresh_synctran_procs is bit, with a default of 1                                           
      , @ignore_distributor        BIT     -- Indicates if this stored procedure executes without connecting to the Distributor. ignore_distributor is bit, with a default of 0                                            
      , @change_active             INT     -- Allows modifying the columns in publications that have subscriptions. change_active is an int with a default of 0                                         
      , @force_invalidate_snapshot BIT     -- Acknowledges that the action taken by this stored procedure may invalidate an existing snapshot. force_invalidate_snapshot is a bit, with a default of 0                                         
      , @force_reinit_subscription BIT     -- Acknowledges that the action taken by this stored procedure may require existing subscriptions to be reinitialized. force_reinit_subscription is a bit, with a default of 0.                                         
      , @publisher                 sysname -- Specifies a non-Microsoft SQL Server Publisher. publisher is sysname, with a default of NULL.                                   
      , @internal                  BIT;    -- Internal use only   

    SELECT
        @publication = [Replication].GetPublicationName (@DatabaseName, @SubscriberInstanceName) -- Changed by M.N. 08 Jul 2015, added @SubscriberInstanceName
      , @operation   = N'drop';

    DECLARE
        @TableName  VARCHAR (256)
      , @SchemaName VARCHAR (256)
      , @ColumnName VARCHAR (256);

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Get all articles into temp table from the relevant database. 
    | Join with [Replication].[Definitions]
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

    DECLARE @SQL NVARCHAR (MAX) = N'USE ' + QUOTENAME(@DatabaseName) + N';
			SELECT
				DB_NAME() AS DatabaseName
				, OBJECT_SCHEMA_NAME(c.OBJECT_ID) AS SchemaName
				, OBJECT_NAME(c.OBJECT_ID) TableName
				, c.name AS ColumnName
--				, c.OBJECT_ID
				, 1
			FROM sys.columns AS c
				INNER JOIN sys.types AS t ON c.user_type_id=t.user_type_id
				INNER JOIN sys.tables b ON c.object_id = b.object_id
				LEFT OUTER JOIN ' + QUOTENAME(DB_NAME()) + N'.[Replication].[ProtectedArticleColumns] a ON a.DatabaseName = DB_NAME() -- Added by M.N. 27 Nov 2017
                    AND a.[SchemaName] = OBJECT_SCHEMA_NAME(c.OBJECT_ID) 
   					AND a.[TableName] = OBJECT_NAME(c.OBJECT_ID)
   					AND a.[ColumnName] = c.name
				INNER JOIN [dbo].[sysarticles] arts ON arts.[objid] = c.OBJECT_ID
				INNER JOIN [dbo].[syspublications] pub ON arts.pubid = pub.pubid				-- Added by M.N. 09 Jul 2015 to filter on publication
			WHERE
				(
					c.max_length = -1 -- VARCHAR(MAX)
					OR c.max_length > 1000 /*Added by Kostya Fridman 18 Nov 2014 */
				)
				AND b.is_ms_shipped = 0
				AND b.type = ''U''
				AND a.[ColumnName] IS NULL
				AND pub.name = ''' + [Replication].GetPublicationName (@DatabaseName, @SubscriberInstanceName) + N'''	-- Added by M.N. 09 Jul 2015 to filter on publication
			ORDER BY 1, 2, 3, 4';
    PRINT @SQL;
    -- Debug
    --SELECT @SQL

    DELETE FROM [Replication].ExcludedArticleColumns
    WHERE IsDynamicalyAdded = 1;
    IF @IsUnLimited = 0
    BEGIN
        INSERT INTO [Replication].ExcludedArticleColumns ( -- Changed By M.N. 25 Jun 2015. It was temporary table
                                                             DatabaseName
                                                           , SchemaName
                                                           , TableName
                                                           , ColumnName
                                                           , IsDynamicalyAdded
                                                         )
        EXEC sys.sp_executesql @SQL;

        /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
        | Add "dropped" columns definitions one by one
        \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
        DECLARE cr_ExcludedColumns CURSOR LOCAL STATIC READ_ONLY FORWARD_ONLY FOR
        SELECT
              SchemaName
            , TableName
            , ColumnName
        FROM  [Replication].ExcludedArticleColumns
        WHERE DatabaseName = @DatabaseName;
        OPEN cr_ExcludedColumns;
        FETCH NEXT FROM cr_ExcludedColumns
        INTO
            @SchemaName
          , @TableName
          , @ColumnName;

        WHILE @@FETCH_STATUS = 0
        BEGIN
            SELECT
                @article = @SchemaName + '.' + @TableName
              , @column  = @ColumnName;

            SELECT @SQL = N'EXEC ' + QUOTENAME(@DatabaseName) + N'.sys.sp_articlecolumn
					  @publication			 	 = @publication			 	   
					, @article                   = @article                     
					, @column                    = @column                      
					, @operation                 = @operation                   
				  --, @refresh_synctran_procs    = @refresh_synctran_procs      
				  --, @ignore_distributor        = @ignore_distributor          
				  --, @change_active             = @change_active               
				  --, @force_invalidate_snapshot = @force_invalidate_snapshot   
				  --, @force_reinit_subscription = @force_reinit_subscription   
				  --, @publisher                 = @publisher                   
				  --, @internal                  = @internal 
				';
            -- debug
            --SELECT @SQL
            --	 , @publication			 	 	 	   
            --	 , @article                              
            --	 , @column                               
            --	 , @operation                 

            -- Print debug information
            PRINT 'Add column `' + @ColumnName + '` to the article `' + @article + '` with flag `dropped = ON` ';

            EXEC sys.sp_executesql
                @SQL
              , N'  @publication			 		sysname                                                     
					, @article						sysname                                        
					, @column						sysname                                        
					, @operation					nvarchar (5)                                     
				  --, @refresh_synctran_procs		bit 
				  --, @ignore_distributor			bit 
				  --, @change_active				int 
				  --, @force_invalidate_snapshot	bit 
				  --, @force_reinit_subscription	bit 
				  --, @publisher					sysname           
				  --, @internal						bit
				'
              , @publication = @publication
              , @article = @article
              , @column = @column
              , @operation = @operation;
            --, @refresh_synctran_procs			= @refresh_synctran_procs      
            --, @ignore_distributor				= @ignore_distributor          
            --, @change_active					= @change_active               
            --, @force_invalidate_snapshot		= @force_invalidate_snapshot   
            --, @force_reinit_subscription		= @force_reinit_subscription   
            --, @publisher						= @publisher                   
            --, @internal						= @internal   

            -- Print debug information
            PRINT 'The column `' + @ColumnName + '` was added to the article `' + @article + '` with flag `dropped = ON`';

            FETCH NEXT FROM cr_ExcludedColumns
            INTO
                @SchemaName
              , @TableName
              , @ColumnName;
        END;

        CLOSE cr_ExcludedColumns;

        DEALLOCATE cr_ExcludedColumns;
    END;
END TRY
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Catch Error:
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
BEGIN CATCH
    IF (
           SELECT CURSOR_STATUS ('local', 'cr_ExcludedColumns')
       ) >= 0
        DEALLOCATE cr_ExcludedColumns;

    IF @@TRANCOUNT > 0
        ROLLBACK TRANSACTION;

    DECLARE @ErrorStr NVARCHAR (4000);
    SELECT @ErrorStr = dbo.FlushErrorString_FN ();
    RAISERROR (@ErrorStr, 16, 1);
    RETURN -1;
END CATCH;
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[06.PublisherRunBackup]'
GO
/******************************************************************************************\ 
DB Object		: Stored Procedure
Objec Name		: [Replication].[06.PublisherRunBackup]
Purpose			: Add articles  step of the transactional replication
Author			: Kostya Fridman
Creation Date	: 18 Nov 2014
-------------------------------------------------------------------------------------------
Demo Call:
	EXEC [Replication].[06.PublisherRunBackup] @DatabaseName = N'TradeNetworks', @SubscriberInstanceName = N'ProdAmsRODB\ROF'

-------------------------------------------------------------------------------------------
Changes History:
================
Author		: Martin Nikolov
Date		: 08 Jul 2015
Description	: added additional input parameter @SubscriberInstanceName
				Additional logick to create the backup only when we process the last publication for the database.
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 30 Jul 2015
Description	: added IsEnabled check
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 16 Sep 2016
Description	: added IsForNY check
-------------------------------------------------------------------------------------------
Author		: Gergana Bivolarska
Approved By : Martin Nikolov
Date		: 21 Jan 2019
Description	: Prepare the backup also when we have more than 1 subscriber for one database 
\******************************************************************************************/
CREATE PROCEDURE [Replication].[06.PublisherRunBackup]
    @DatabaseName           NVARCHAR (256)
  , @SubscriberInstanceName NVARCHAR (256) -- Added by M.N. 08 Jul 2015
AS
SET NOCOUNT ON;

BEGIN TRY

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Server validation
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    IF NOT EXISTS (
                      SELECT TOP 1
                             1
                      FROM   [Replication].Definitions
                      WHERE
                             DatabaseName           = @DatabaseName
                         AND SubscriberInstanceName = @SubscriberInstanceName -- Added by M.N. 08 Jul 2015
                         AND IsEnabled              = 1 -- Added by M.N. 30 Jul 2015
                         AND @@SERVERNAME           = PublisherInstanceName
                  )
    BEGIN
        RAISERROR ('Warning! You are trying to execute SP on wrong server, or with wrong combination DatabaseName/SubscriberInstanceName!
      Please check [PublisherInstanceName], [DatabaseName], [SubscriberInstanceName], [IsEnabled] in [Replication].[Definitions]!', 16, 1);
        RETURN -1;
    END;

    --/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    --| Added by M.N. 10 Jul 2015 to cove the backup issue when we have more than 1 subscriber for one database 
    --| We need to make the backup when we process the last publication
    --\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    --    SELECT SubscriberInstanceName
    --    FROM   [Replication].Definitions
    --    WHERE
    --           DatabaseName                                            = @DatabaseName
    --       AND SubscriberInstanceName                                  = @SubscriberInstanceName --Added by G.B. 21 Jan 2019
    --       AND PublisherProcessed                                      = 0
    --       AND IsEnabled                                               = 1 -- Added by M.N. 30 Jul 2015
    --       AND CASE WHEN @@SERVERNAME LIKE 'PRODNY%' THEN 1 ELSE 0 END = IsForNY; -- Added by M.N. 16 Sep 2016

    --Removed By G.B. 21 Jan 2019
    --IF (@@ROWCOUNT > 1)
    --BEGIN
    --    RAISERROR ('Warning! Backup is not created because we have additional subscribers for that database!', 8, 1);
    --    RETURN 0;
    --END;
    /* Adeed by M.N. 10 Jul 2015 TO HERE */

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Run Backup
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    DECLARE @SQL NVARCHAR (MAX) = N'BACKUP DATABASE ' + QUOTENAME(@DatabaseName) + N' 
	TO DISK = '''                 + [Replication].GetBackupFileAddressOnPublisher (@DatabaseName, @SubscriberInstanceName) -- Changed by M.N. 08 Jul 2015, added @SubscriberInstanceName
                                  + N'''
	  WITH NOFORMAT
	-- Drop existing backup --, COPY_ONLY
	, INIT
	, NAME = N'''                 + @DatabaseName + N'-Full Database Backup''
	, SKIP
	, NOREWIND
	, NOUNLOAD
	, COMPRESSION
	, STATS = 1;';

    -- debug: 
    PRINT @SQL;

    EXEC sys.sp_executesql @SQL;

    SELECT 'FYI: The backup was created under "' + [Replication].GetBackupFileAddressOnPublisher (@DatabaseName, @SubscriberInstanceName) + '" '; -- Changed by M.N. 08 Jul 2015, added @SubscriberInstanceName

END TRY
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Catch Error:
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
BEGIN CATCH
    IF @@TRANCOUNT > 0
        ROLLBACK TRANSACTION;

    DECLARE @ErrorStr NVARCHAR (4000);
    SELECT @ErrorStr = dbo.FlushErrorString_FN ();
    RAISERROR (@ErrorStr, 16, 1);
    RETURN -1;
END CATCH;
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[01.AddLoginToAdminGroup]'
GO
/******************************************************************************************\ 
DB Object		: Stored Procedure
Objec Name		: [Replication].[01.AddLoginToAdminGroup]
Purpose			: If login [Replication].[Definitions].[WindowsLoginName] is missing - created it.
					!! It should be executed on Publisher, Subscriber & Distributor
Author			: Kostya Fridman
Creation Date	: 18 Nov 2014
-------------------------------------------------------------------------------------------
Demo Call: 

	EXEC [Replication].[01.AddLoginToAdminGroup] 
		@DatabaseName = N'TradeNetworks'
		, @SubscriberInstanceName = N'ProdAmsRODB\ROF'
		, @RunFlag = 1
		, @PrintFlag = 0

	EXEC [Replication].[01.AddLoginToAdminGroup] 
		@DatabaseName = N'TradeNetworks'
		, @SubscriberInstanceName = N'ProdAmsRODB\ROF'
		, @RunFlag = 0
		, @PrintFlag = 1

-------------------------------------------------------------------------------------------
Changes History:
================
Author		: Martin Nikolov
Date		: 08 Jul 2015
Description	: added additional input parameter @SubscriberInstanceName
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 30 Jul 2015
Description	: added IsEnabled check
-------------------------------------------------------------------------------------------
Author		: Gergana Bivolarska
Approved By : Martin Nikolov
Date		: 07 Jan 2019
Description	: Added output parameter
\******************************************************************************************/
CREATE PROCEDURE [Replication].[01.AddLoginToAdminGroup]
    @DatabaseName           NVARCHAR (256)
  , @SubscriberInstanceName NVARCHAR (256)             -- Added by M.N. 08 Jul 2015
  , @RunFlag                BIT            = NULL      -- Is `1` when we run for Publisher. 0 o/w
  , @PrintFlag              BIT            = NULL      -- Is `1` when we run for Distributor Or Subscriber. 0 o/w
  , @SPOutput               NVARCHAR (MAX) = '' OUTPUT --Added by Gergana Bivolarska 07 Jan 2019
AS
SET NOCOUNT ON;

BEGIN TRY

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Server validation
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    IF NOT EXISTS (
                      SELECT TOP 1
                             1
                      FROM   [Replication].Definitions
                      WHERE
                             DatabaseName           = @DatabaseName
                         AND SubscriberInstanceName = @SubscriberInstanceName -- Added by M.N. 08 Jul 2015
                         AND IsEnabled              = 1 -- Added by M.N. 30 Jul 2015
                         AND @@SERVERNAME           = PublisherInstanceName
                  )
    BEGIN
        RAISERROR ('Warning! You are trying to execute SP on wrong server, or with wrong combination DatabaseName/SubscriberInstanceName!
      Please check [PublisherInstanceName], [DatabaseName], [SubscriberInstanceName], [IsEnabled] in [Replication].[Definitions]!', 16, 1);
        RETURN -1;
    END;

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Add admin's login to the server, when missing
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    DECLARE @WindowsLoginName VARCHAR (200) = (
                                                  SELECT WindowsLoginName
                                                  FROM   [Replication].Definitions
                                                  WHERE
                                                         DatabaseName           = @DatabaseName
                                                     AND SubscriberInstanceName = @SubscriberInstanceName -- Added by M.N. 08 Jul 2015
                                              );

    DECLARE @SQL NVARCHAR (MAX) = N'';

    SELECT @SQL = N'
USE [master];
IF NOT EXISTS( SELECT TOP 1 1 FROM [sys].[syslogins] WHERE [loginname] = ''' + @WindowsLoginName + N''' )
BEGIN
	CREATE LOGIN [' + @WindowsLoginName + N'] FROM WINDOWS 
	WITH DEFAULT_DATABASE=[master], DEFAULT_LANGUAGE=[us_english];
END';

    IF @RunFlag = 1
    BEGIN
        EXEC sys.sp_executesql @SQL;
    END;

    IF @PrintFlag = 1
    BEGIN
        SELECT @SPOutput = @SQL + N'
GO
'       ;
    END;

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Add login to the goup of admins 
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

    SELECT @SQL = N'USE [master];
 IF NOT EXISTS (
	SELECT TOP 1 1
	FROM sys.server_role_members
		JOIN sys.server_principals AS role ON role_principal_id = role.principal_id
		JOIN sys.server_principals AS member ON member_principal_id = member.principal_id
	WHERE role.name = ''sysadmin''
		AND member.name = ''' + @WindowsLoginName + N'''
)
BEGIN
	ALTER SERVER ROLE [sysadmin] ADD MEMBER  [' + @WindowsLoginName + N'];
END';

    IF @RunFlag = 1
    BEGIN
        EXEC sys.sp_executesql @SQL;
    END;

    IF @PrintFlag = 1
    BEGIN
        SELECT @SPOutput = @SQL + N'
GO
'       ;
    END;


END TRY
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Catch Error:
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
BEGIN CATCH
    IF @@TRANCOUNT > 0
        ROLLBACK TRANSACTION;

    DECLARE @ErrorStr NVARCHAR (4000);
    SELECT @ErrorStr = dbo.FlushErrorString_FN ();
    RAISERROR (@ErrorStr, 16, 1);
    RETURN -1;
END CATCH;
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[Step1_DistributerCreateAllObjects]'
GO
/******************************************************************************************\ 
DB Object		: Stored Procedure
Objec Name		: [Replication].[Step1_DistributerCreateAllObjects]
Purpose			: First step of the transactional replication creation
					This step is used to create the distribution database 
Author			: Kostya Fridman and Ivo Ivanov
Creation Date	: 18 Nov 2014
-------------------------------------------------------------------------------------------
Demo Call:
   -- Important: It should be run on the Distributor machine!
	EXEC [Replication].[Step1_DistributerCreateAllObjects] @DatabaseName = N'TradeNetworks', @SubscriberInstanceName = N'SOFDBA\DBA'

-------------------------------------------------------------------------------------------
Changes History:
================
Author		: Ivo Ivanov
Date		: 19 Nov 2014
Description	: Preparing a dynamic SQL code for the main part of the procedure. 
				Checking the @@SERVERNAME at the begining.
-------------------------------------------------------------------------------------------
Author		: Kostya Fridman
Date		: 21 Nov 2014
Description	: the call to [Replication].[01.AddLoginToAdminGroup]  was added       
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 08 Jul 2015
Description	: added additional input parameter @SubscriberInstanceName
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 30 Jul 2015
Description	: added checks if we have the server already defined as distributor
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 21 Dec 2015
Description	: Change the schedule of "Distribution clean up: distribution JOB to once per day.
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 12 Dec 2017
Description	: Change the schedule of "Distribution clean up: distribution JOB to 22:30 and max_distretention = 24.
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 21 Mar 2018
Description	: Added the distributor DB data_folder and log_folder in Replication.Definitions
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 07 Aug 2018
Description	: For the local env 'Distribution clean up: distribution' will be scheduled every 30 min
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 04 Feb 2019
Approved By : Gergana Bivolarska
Description	: For all env 'Distribution clean up: distribution' will be scheduled every 30 min
-------------------------------------------------------------------------------------------
Author		: 
Date		: 
Description	: 
\******************************************************************************************/
CREATE PROCEDURE [Replication].[Step1_DistributerCreateAllObjects]
    @DatabaseName           NVARCHAR (256)
  , @SubscriberInstanceName NVARCHAR (256) -- Added by M.N. 08 Jul 2015
AS
SET NOCOUNT ON;
DECLARE @OUTPUT NVARCHAR(MAX) = ''

BEGIN TRY

    SELECT @OUTPUT += '
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- Run returned string on DISTRIBUTER !!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
'   ;

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Print script which adds permissions to Distributor 
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    EXEC [Replication].[01.AddLoginToAdminGroup]
        @DatabaseName = @DatabaseName
      , @SubscriberInstanceName = @SubscriberInstanceName -- Added by M.N. 08 Jul 2015
      , @RunFlag = 0
      , @PrintFlag = 1;

    SELECT @OUTPUT += N'
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
| Check if the procedure is executed on the Distributor machine
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
IF (@@SERVERNAME NOT LIKE ''' + DistributerInstanceName + N''' )
BEGIN
	RAISERROR( ''Warning! You are trying to execute script on a wrong server!
		Please use distributor or check your Distributor`s name!'' , 16, 1)
END
ELSE 
BEGIN
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
| Check if we allready have distribution DB : added by M.N. 30 Jul 2015
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
	IF DB_ID(''distribution'') IS NOT NULL
	BEGIN
		RAISERROR( ''Warning! Distributor is already initialized (there is [distribution] DB)!
			If you want to reinitialize it please clean it first!'' , 16, 1)
		RETURN
	END
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
| Add the distributor and distribution databases
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
	DECLARE
		@DistributorName varchar(256)

	SELECT
		  @DistributorName = ''' + DistributerInstanceName + N'''

	-- Check if the distributor was not created
	EXEC master.sys.sp_adddistributor @distributor = @DistributorName;

	-- Check if the database doesn''t exsts
	EXEC master.sys.sp_adddistributiondb
		@database = N''distribution''' + CASE WHEN DistributorMDFFileLocationFolder IS NOT NULL
                                              THEN N', @data_folder = N''' + DistributorMDFFileLocationFolder + N''''
                                              ELSE N''
                                         END + CASE WHEN DistributorLDFFileLocationFolder IS NOT NULL
                                                    THEN N', @log_folder = N''' + DistributorLDFFileLocationFolder + N''''
                                                    ELSE N''
                                               END + N'
		, @min_distretention = 0
		, @max_distretention = 24
		, @history_retention = 12
		, @security_mode = 1 -- means Window Integration Security

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
| Change the schedule of Distribution clean up: distribution JOB to once per day.
| Added by M.N. 21 Dec 2015
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
	DECLARE
		@schedule_id INT

	SELECT
		@schedule_id = JS.schedule_id
	FROM msdb.dbo.sysjobs J
		INNER JOIN  msdb.dbo.sysjobschedules JS ON J.job_id = JS.job_id
	WHERE j.name = ''Distribution clean up: distribution''

	EXEC msdb.dbo.sp_update_schedule
		@schedule_id = @schedule_id
        , @freq_type = 4
        , @freq_interval = 1
        , @freq_subday_type = 4
        , @freq_subday_interval = 30
        , @freq_relative_interval = 1
        , @freq_recurrence_factor = 0
		, @active_start_date=20170101
		, @active_end_date=99991231
		, @active_start_time=000000
		, @active_end_time=235959
END
GO'
    FROM   [Replication].Definitions
    WHERE
           DatabaseName           = @DatabaseName
       AND SubscriberInstanceName = @SubscriberInstanceName; -- Added by M.N. 08 Jul 2015

    SELECT @OUTPUT += N'
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
| Check if the procedure is executed on the Distributor machine
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
IF (@@SERVERNAME NOT LIKE ''' + DistributerInstanceName + N''' )
BEGIN
	RAISERROR( ''Warning! You are trying to execute script on a wrong server!
		Please use distributor or check your Distributor`s name!'' , 16, 1)  
END
ELSE 
BEGIN
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
| Check if we allready this publisher defined allready : added by M.N. 30 Jul 2015
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
	IF EXISTS (SELECT TOP 1 1 FROM msdb.dbo.MSdistpublishers WHERE name = ''' + PublisherInstanceName + N''')
	BEGIN
		RAISERROR( ''Warning! The server ''''' + PublisherInstanceName + N''''' is already listed as a Publisher!'' , 16, 1)
		RETURN
	END
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
| Add the distributor and distribution databases
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
	DECLARE
		  @DistributorName varchar(256)
		, @PublisherName varchar(256)

	SELECT 
		  @DistributorName = ''' + DistributerInstanceName + N'''
		, @PublisherName = ''' + PublisherInstanceName + N'''
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
| Create table UIProperties on Distribution database
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
	-- It should be dynamic SQL
	-- Run it on the DB "[distribution]"
	USE [distribution] 
	IF (NOT EXISTS (SELECT * FROM sysobjects WHERE name = ''UIProperties'' AND type = ''U '')) 
		CREATE TABLE UIProperties(id int) 
	IF (EXISTS (SELECT * FROM ::fn_listextendedproperty(''SnapshotFolder'', ''user'', ''dbo'', ''table'', ''UIProperties'', null, null))) 
		EXEC sp_updateextendedproperty N''SnapshotFolder'', NULL, ''user'', dbo, ''table'', ''UIProperties'' 
	ELSE 
		EXEC sp_addextendedproperty    N''SnapshotFolder'', NULL, ''user'', dbo, ''table'', ''UIProperties''

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
| Make relation between Distributor and Publisher machines
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
	EXEC sp_adddistpublisher
		  @publisher = @PublisherName
		, @distribution_db = N''distribution''
		, @security_mode = 1
		, @working_directory = ''' + DistributorWorkingDirectory + N'''
		, @trusted = N''false''
		, @thirdparty_flag = 0
		, @publisher_type = N''MSSQLSERVER''

	USE distribution;
	EXEC sp_changedistributor_password @password = ''' + DistributorPassword + N''' 
END'
    FROM   [Replication].Definitions
    WHERE
           DatabaseName           = @DatabaseName
       AND SubscriberInstanceName = @SubscriberInstanceName; -- Added by M.N. 08 Jul 2015

    SELECT CAST(@OUTPUT AS XML);


END TRY
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Catch Error:
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
BEGIN CATCH
    IF @@TRANCOUNT > 0
        ROLLBACK TRANSACTION;

    DECLARE @ErrorStr NVARCHAR (4000);
    SELECT @ErrorStr = dbo.FlushErrorString_FN ();
    RAISERROR (@ErrorStr, 16, 1);
    RETURN -1;
END CATCH;
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[Step4_SubscriberCleanObjects]'
GO
/******************************************************************************************\ 
DB Object		: Stored Procedure
Objec Name		: [Replication].[Step4_SubscriberCleanObjects]
Purpose			: This step is used to set Allow "NULL" flags, disable triggers and drop FKs
Author			: Kostya Fridman
Creation Date	: 19 Nov 2014
-------------------------------------------------------------------------------------------
Demo Call:
	-- Important: It should be run on the Subscriber machine!

	EXEC [Replication].Step4_SubscriberCleanObjects
		@DatabaseName = N'Configuration'
		, @SubscriberInstanceName = N'SOFDBA\DBA'

-------------------------------------------------------------------------------------------
Changes History:
================ 
Author		: Kostya Fridman
Date		: 21 Nov 2014
Description	: the call to [Replication].[01.AddLoginToAdminGroup]  was added 
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 24 Jun 2015
Description	: Make columns defined in [Replication].ExcludedArticleColumns IsDynamicalyAdded = 0 NULLable
				Removed ReadOnly user.
				Tran log max size 40GB.
				Execute [Security].[FixPermisions] for database (Application and Debug)
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 08 Jul 2015
Description	: added additional input parameter @SubscriberInstanceName
				Added restore filelistonly to take the list of files from the backup for the move part of the restor
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 30 Jul 2015
Description	: added IsEnabled check
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 09 Mar 2016
Description	: DROP ALL Check Constraints from the subscriber (they are checked on the publisher)
				And Added @UseDefaultDomainColum = 1 on EXEC [Security].[FixPermisions]
-------------------------------------------------------------------------------------------
Author		: Kostya Fridman
Date		: 25 Dec 2016
Description	: #63641 - the call to SP_EXECUTESQL was changed
			 - an addtional columns was added because switch to SQL 2016
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 07 Sep 2017
Description	: Added additional check for the columns that we change from NOT NULL to NULL
				Now we check that the column is not in [Replication].[ProtectedArticleColumns]
-------------------------------------------------------------------------------------------
Changed By  : Martin Nikolov
Changed Date: 29 Oct 2018
Approved By : 
Description	: #86238 - Switch the recovery of the DB on the SUBSCRIBER to simple
-------------------------------------------------------------------------------------------
Changed By  : Gergana Bivolarska 
Changed Date: 19 Dec 2018
Approved By : Martin Nikolov
Description	: Added isUnlimited column in [Replication].[Definitions] table for AWS 
-------------------------------------------------------------------------------------------
Changed By  : Gergana Bivolarska 
Changed Date: 16 Jan 2019
Approved By : Martin Nikolov
Description	: №87654 - Create table to keep all FK and CK
\******************************************************************************************/
CREATE PROCEDURE [Replication].[Step4_SubscriberCleanObjects]
    @DatabaseName           NVARCHAR (256)
  , @SubscriberInstanceName NVARCHAR (256) -- Added by M.N. 08 Jul 2015
AS
SET NOCOUNT ON;

BEGIN TRY

    DECLARE
        @SQL      NVARCHAR (MAX) = N''
      , @SPOutput NVARCHAR (MAX) = N''
      , @SPExec   NVARCHAR (MAX) = N'';

    --Added by Gergana Bivolarska 17 Dec 2018
    DECLARE @IsUnLimited BIT = 0;
    SET @IsUnLimited = (
                           SELECT IsUnLimited
                           FROM   [Replication].Definitions
                           WHERE
                                  DatabaseName           = @DatabaseName
                              AND SubscriberInstanceName = @SubscriberInstanceName
                              AND IsEnabled              = 1
                       );

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Server validation
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    IF NOT EXISTS (
                      SELECT TOP 1
                             1
                      FROM   [Replication].Definitions
                      WHERE
                             DatabaseName           = @DatabaseName
                         AND SubscriberInstanceName = @SubscriberInstanceName -- Added by M.N. 08 Jul 2015
                         AND IsEnabled              = 1 -- Added by M.N. 30 Jul 2015
                         AND @@SERVERNAME           = PublisherInstanceName
                  )
    BEGIN
        RAISERROR ('Warning! You are trying to execute SP on wrong server, or with wrong combination DatabaseName/SubscriberInstanceName!
      Please check [PublisherInstanceName], [DatabaseName], [SubscriberInstanceName], [IsEnabled] in [Replication].[Definitions]!', 16, 1);
        RETURN -1;
    END;

    SELECT @SQL += N'
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- Run returned string on SUBSCRIBER!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
'   ;

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Print script which adds permissions to Subscriber 
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    EXEC [Replication].[01.AddLoginToAdminGroup]
        @DatabaseName = @DatabaseName
      , @SubscriberInstanceName = @SubscriberInstanceName -- Added by M.N. 08 Jul 2015
      , @RunFlag = 0
      , @PrintFlag = 1
      , @SPOutput = @SQL OUTPUT;

    SELECT @SQL += @SPOutput;

    /* FROM HERE ADDED BY M.N. 08 Jul 2015 */
    DECLARE
        @MDFFileLocationFolder NVARCHAR (255)
      , @LDFFileLocationFolder NVARCHAR (255)
      , @RestoreFileListCMD    NVARCHAR (256)
      , @RestoreMoveCMD        NVARCHAR (MAX) = N'';

    SELECT
         @MDFFileLocationFolder = SubscriberMDFFileLocationFolder
       , @LDFFileLocationFolder = SubscriberLDFFileLocationFolder
    FROM [Replication].Definitions
    WHERE
         DatabaseName           = @DatabaseName
     AND SubscriberInstanceName = @SubscriberInstanceName;

    DECLARE @Table TABLE (
        LogicalName          NVARCHAR (128)
      , PhysicalName         NVARCHAR (260)
      , Type                 CHAR (1)
      , FileGroupName        NVARCHAR (128)
      , Size                 NUMERIC (20, 0)
      , MaxSize              NUMERIC (20, 0)
      , FileID               BIGINT
      , CreateLSN            NUMERIC (25, 0)
      , DropLSN              NUMERIC (25, 0)  NULL
      , UniqueID             UNIQUEIDENTIFIER
      , ReadOnlyLSN          NUMERIC (25, 0)  NULL
      , ReadWriteLSN         NUMERIC (25, 0)  NULL
      , BackupSizeInBytes    BIGINT
      , SourceBlockSize      INT
      , FileGroupID          INT
      , LogGroupGUID         UNIQUEIDENTIFIER NULL
      , DifferentialBaseLSN  NUMERIC (25, 0)  NULL
      , DifferentialBaseGUID UNIQUEIDENTIFIER
      , IsReadOnly           BIT
      , IsPresent            BIT
      , TDEThumbprint        VARBINARY (32)
      , SnapshotURL          NVARCHAR (1000) -- Added by KF 25 Dec 2014 because of the switch to SQL 2016
    );

    SELECT @RestoreFileListCMD = N'RESTORE FILELISTONLY FROM DISK = N''' + [Replication].GetBackupFileAddressOnSubscriber (@DatabaseName, @SubscriberInstanceName) + N'''';

    INSERT INTO @Table
    EXEC sys.sp_executesql @RestoreFileListCMD;

    SELECT @RestoreMoveCMD +=+CHAR (10) + CHAR (9) + N', MOVE N''' + LogicalName + N''' TO N''' + CASE WHEN Type = 'L'
                                                                                                       THEN @LDFFileLocationFolder + CASE WHEN RIGHT(@LDFFileLocationFolder, 1) = '\' THEN N'' ELSE N'\' END
                                                                                                       ELSE @MDFFileLocationFolder + CASE WHEN RIGHT(@MDFFileLocationFolder, 1) = '\' THEN N'' ELSE N'\' END
                                                                                                  END + REVERSE (LEFT(REVERSE (PhysicalName), CHARINDEX ('\', REVERSE (PhysicalName)) - 1)) + N''''
    FROM   @Table;

    /* TO HERE ADDED BY M.N. 08 Jul 2015 */

    SELECT @SQL += N'USE [master];
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Check if the procedure is executed on the subscriber machine
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
IF (@@SERVERNAME NOT LIKE ''' + @SubscriberInstanceName + N''' )
BEGIN
	RAISERROR( ''Warning! You are trying to execute script on a wrong server!
		Please use subscriber or check your Subscriber`s name in ' + QUOTENAME(DB_NAME()) + N'.[Replication].[Definitions]!'' , 16, 1)
	RETURN
END

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| kill all User processes!
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
DECLARE @SQL NVARCHAR(4000) = N''''

SELECT @SQL = @SQL + ''kill ''+ cast(spid as varchar(10)) + '';'' + char(10)
FROM [master].[sys].[sysprocesses]
WHERE
	DB_NAME(dbid) = ''' + @DatabaseName + N'''

SELECT @SQL
IF @SQL NOT LIKE '''' 
BEGIN
	PRINT ''kill all User processes!''
	EXEC SP_EXECUTESQL @SQL
END
'   ;
    SELECT @SQL += N'
RESTORE DATABASE ' + QUOTENAME(@DatabaseName) + N' 
FROM DISK = N''' + [Replication].GetBackupFileAddressOnSubscriber (@DatabaseName, @SubscriberInstanceName) -- Changed by M.N. 08 Jul 2015, added @SubscriberInstanceName
+       N''' WITH FILE = 1' + @RestoreMoveCMD + N'
	, NOUNLOAD
	, REPLACE
	, STATS = 1;
 GO';

    --Added by Gergana Bivolarska 15 Jan 2019
    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Keep ALL FKs into Configuration table
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    SELECT @SQL += N'
DROP TABLE IF EXISTS ' + QUOTENAME(DB_NAME()) + N'.DR.RecreateObjects

CREATE TABLE ' + QUOTENAME(DB_NAME()) + N'.DR.RecreateObjects (
          DatabaseName NVARCHAR(256) NOT NULL
        , ObjectTypeId TINYINT       NOT NULL
        , SchemaName   NVARCHAR(256) NOT NULL
        , ObjectName   NVARCHAR(256) NOT NULL
        , CreateScript NVARCHAR(MAX) NOT NULL
        , CONSTRAINT PK_RecreateObjects PRIMARY KEY CLUSTERED (DatabaseName, ObjectTypeId, SchemaName, ObjectName)'
+ CASE WHEN OBJECT_ID(QUOTENAME(DB_NAME()) + N'.DR.ObjectTypes') IS NOT NULL THEN N'
		, CONSTRAINT [FK_RecreateObjectsObjectTypeId_ObjectTypesObjectTypeId] FOREIGN KEY (ObjectTypeId) REFERENCES ' + QUOTENAME(DB_NAME()) + N'.DR.ObjectTypes (ObjectTypeId)'
	ELSE N'' END + N'
);
GO

USE ' + QUOTENAME (@DatabaseName) + N'
GO

INSERT INTO ' + QUOTENAME(DB_NAME()) + N'.DR.RecreateObjects (
                                          DatabaseName
                                        , ObjectTypeId
                                        , SchemaName
                                        , ObjectName
                                        , CreateScript
                                      )
            SELECT
                  DB_NAME()                  AS DatabaseName
                , 1 /*FK*/                   AS ObjectTypeId
                , ParentTableSchemaName      AS SchemaName
                , ForeignKeyName             AS ObjectName
                , N''ALTER TABLE ['' + ParentTableSchemaName + N''].['' + ParentTableName + N''] WITH CHECK ADD CONSTRAINT ['' + ForeignKeyName + N''] FOREIGN KEY ('' + ParentColumnName + N'') REFERENCES ['' + ReferencedTableSchemaName + N''].['' + ReferencedTableName + N''] ('' + ReferencedColumnName + N'');'' AS CreateScript
            FROM (
                     SELECT    DISTINCT
                               ParentTableSchema.name     AS ParentTableSchemaName
                             , ParentTable.name           AS ParentTableName
                             , fk.name                    AS ForeignKeyName
                             , STUFF ((
                                          SELECT    '', '' + c2.name
                                          FROM
                                                    sys.columns             AS c2
                                         INNER JOIN sys.foreign_key_columns AS fkc
                                                 ON c2.object_id = fkc.referenced_object_id
                                                AND c2.column_id = fkc.referenced_column_id
                                          WHERE     fkc.constraint_object_id = fk.object_id
                                          ORDER BY  fkc.constraint_column_id
                                          FOR XML PATH ('''')
                                      ), 1, 2, ''''
                                     )                    AS ReferencedColumnName
                             , ReferencedTableSchema.name AS ReferencedTableSchemaName
                             , ReferencedTable.name       AS ReferencedTableName
                             , STUFF ((
                                          SELECT    '', '' + c1.name
                                          FROM
                                                    sys.columns             AS c1
                                         INNER JOIN sys.foreign_key_columns AS fkc
                                                 ON c1.object_id = fkc.parent_object_id
                                                AND c1.column_id = fkc.parent_column_id
                                          WHERE     fkc.constraint_object_id = fk.object_id
                                          ORDER BY  fkc.constraint_column_id
                                          FOR XML PATH ('''')
                                      ), 1, 2, ''''
                                     )                    AS ParentColumnName
                     FROM
                               sys.foreign_keys        AS fk
                    INNER JOIN sys.foreign_key_columns AS fkc
                            ON fk.object_id              = fkc.constraint_object_id
                    INNER JOIN sys.tables              AS ParentTable
                            ON ParentTable.object_id     = fk.parent_object_id
                    INNER JOIN sys.schemas             AS ParentTableSchema
                            ON ParentTable.schema_id     = ParentTableSchema.schema_id
                    INNER JOIN sys.tables              AS ReferencedTable
                            ON fk.referenced_object_id   = ReferencedTable.object_id
                    INNER JOIN sys.schemas             AS ReferencedTableSchema
                            ON ReferencedTable.schema_id = ReferencedTableSchema.schema_id
                 ) AS X;
GO' +   CHAR (10);

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Keep ALL CK into Configuration table
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    SELECT @SQL = @SQL + N'
INSERT INTO ' + QUOTENAME(DB_NAME()) + N'.DR.RecreateObjects (
                                                 DatabaseName
                                               , ObjectTypeId
                                               , SchemaName
                                               , ObjectName
                                               , CreateScript
                                             )
            SELECT    DISTINCT
                      DB_NAME()                                                                                                                                         AS DatabaseName
                    , 2 /*CK*/                                                                                                                                          AS ObjectTypeId
                    , OBJECT_SCHEMA_NAME (so.parent_obj)                                                                                                                AS SchemaName
                    , cc.CONSTRAINT_NAME                                                                                                                                AS ObjectName
                    , N''ALTER TABLE  '' + QUOTENAME (OBJECT_SCHEMA_NAME (so.parent_obj)) + N''.'' + QUOTENAME (OBJECT_NAME (so.parent_obj))
                                              + CHAR (10) + N'' WITH CHECK ADD CONSTRAINT '' + QUOTENAME (cc.CONSTRAINT_NAME) + N'' CHECK ('' + cc.CHECK_CLAUSE + '')'' AS CreateScript
            FROM
                      ' + QUOTENAME (@DatabaseName) + N'.INFORMATION_SCHEMA.CHECK_CONSTRAINTS AS cc
           INNER JOIN ' + QUOTENAME (@DatabaseName) + N'.sys.sysobjects                       AS so
                   ON cc.CONSTRAINT_NAME = so.name;
GO'               + CHAR (10);

    SELECT @SQL += N' 
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Check if the procedure is executed on the Distributor machine
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
IF (@@SERVERNAME NOT LIKE ''' + @SubscriberInstanceName + N''' )
BEGIN
	RAISERROR( ''Warning! You are trying to execute script on a wrong server!
		Please use subscriber or check your Subscriber`s name in ' + QUOTENAME(DB_NAME()) + N'.[Replication].[Definitions]!'' , 16, 1)
	RETURN
END

DECLARE @SQL NVARCHAR(MAX) = ''''';

    SELECT @SQL += N'
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| DROP ALL FKs
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
SELECT @SQL = @SQL + N''
ALTER TABLE ['' + TABLE_SCHEMA + N''].['' + TABLE_NAME + N'']  DROP CONSTRAINT ['' + CONSTRAINT_NAME + N''];''
FROM   INFORMATION_SCHEMA.TABLE_CONSTRAINTS
WHERE
       CONSTRAINT_TYPE = ''FOREIGN KEY''
   AND TABLE_NAME NOT LIKE ''syncobj[_]0x%'';

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| DISABLE ALL TRIGGERS
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
SELECT @SQL = @SQL + N''
DISABLE TRIGGER ['' + Z.TriggerSchemaName + N''].['' + Z.TriggerName + N''] ON ['' + Z.TableSchemaName + N''].['' + Z.TableName + N''];''
FROM   (
           SELECT TOP 1000
                  name                           AS TriggerName
                , OBJECT_NAME (parent_id)        AS TableName
                , OBJECT_SCHEMA_NAME (parent_id) AS TableSchemaName
                , OBJECT_SCHEMA_NAME (object_id) AS TriggerSchemaName
           FROM   sys.triggers
           WHERE
                  OBJECT_NAME (object_id) IS NOT NULL
              AND OBJECT_SCHEMA_NAME (object_id) IS NOT NULL
       ) AS Z;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| DROP ALL CKs
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
SELECT     @SQL = @SQL + N''
ALTER TABLE ['' + s.name + N''].['' + o.name + N'']  DROP CONSTRAINT ['' + cc.name + N''];''
FROM
           sys.check_constraints AS cc
INNER JOIN sys.objects           AS o
        ON cc.parent_object_id = o.object_id
INNER JOIN sys.schemas           AS s
        ON cc.schema_id        = s.schema_id;
'   ;

    IF @IsUnLimited = 0
    BEGIN
        SELECT @SPExec += N'
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| ALLOW NULLs WHERE COLUMN''S TYPE IS: XML NOT NULL
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
SELECT @SPOutput += ''
ALTER TABLE ['' + [TABLE_SCHEMA] + ''].['' + [TABLE_NAME] + ''] ALTER COLUMN ['' + [COLUMN_NAME] + ''] ['' + UPPER([DATA_TYPE]) + ''] NULL;''
FROM [' +   @DatabaseName + N'].[INFORMATION_SCHEMA].[COLUMNS]  C
WHERE
	[DATA_TYPE] = ''xml'' 
	AND [IS_NULLABLE] = ''NO'' 
	AND [TABLE_NAME] NOT LIKE ''syncobj[_]0x%''
	AND EXISTS (
		SELECT TOP 1 1 -- Skip views
		FROM ' + QUOTENAME(@DatabaseName) + N'.[INFORMATION_SCHEMA].[TABLES] Z
		WHERE
			Z.[TABLE_TYPE] = ''BASE TABLE''
			AND Z.[TABLE_NAME] = C.[TABLE_NAME] 
			AND Z.[TABLE_SCHEMA] = C.[TABLE_SCHEMA]
	)
	AND NOT EXISTS (
		SELECT TOP 1 1 
		FROM ' + QUOTENAME(DB_NAME()) + N'.[Replication].ProtectedArticleColumns AS PAC
		WHERE
			    PAC.DatabaseName = C.TABLE_CATALOG
			AND PAC.SchemaName = C.TABLE_SCHEMA
			AND PAC.TableName = C.[TABLE_NAME]
			AND PAC.ColumnName = C.COLUMN_NAME
		) 
'       ;

        SELECT @SPExec += N'
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| ALLOW NULLs WHERE COLUMN''S TYPE IS: TEXT\VARCHAR (MAX) NOT NULL
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
SELECT @SPOutput += ''
ALTER TABLE ['' + [TABLE_SCHEMA] + ''].['' + [TABLE_NAME] + ''] ALTER COLUMN ['' + [COLUMN_NAME] + ''] ['' + UPPER([DATA_TYPE]) + ''] (MAX) NULL;''
FROM  [' +  @DatabaseName + N'].[INFORMATION_SCHEMA].[COLUMNS] C
WHERE
	[DATA_TYPE] NOT LIKE ''xml'' 
	AND [CHARACTER_MAXIMUM_LENGTH] = -1
	AND [IS_NULLABLE] = ''NO'' 
	AND [TABLE_NAME] NOT LIKE ''syncobj[_]0x%''
	AND EXISTS (
		SELECT TOP 1 1 -- Skip views
		FROM ' + QUOTENAME(@DatabaseName) + N'.[INFORMATION_SCHEMA].[TABLES] Z
		WHERE
			Z.[TABLE_TYPE] = ''BASE TABLE''
			AND Z.[TABLE_NAME] = C.[TABLE_NAME] 
			AND Z.[TABLE_SCHEMA] = C.[TABLE_SCHEMA]
	)
	AND NOT EXISTS (
		SELECT TOP 1 1 
		FROM ' + QUOTENAME(DB_NAME()) + N'.[Replication].ProtectedArticleColumns AS PAC
		WHERE
			    PAC.DatabaseName = C.TABLE_CATALOG
			AND PAC.SchemaName = C.TABLE_SCHEMA
			AND PAC.TableName = C.[TABLE_NAME]
			AND PAC.ColumnName = C.COLUMN_NAME
		) 
'       ;

        SELECT @SPExec += N'
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| ALLOW NULLs WHERE COLUMN''S TYPE IS: TEXT\VARCHAR (> 1000) NOT NULL
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
SELECT @SPOutput += ''
ALTER TABLE ['' + [TABLE_SCHEMA] + ''].['' + [TABLE_NAME] + ''] ALTER COLUMN ['' + [COLUMN_NAME] + ''] ['' + UPPER([DATA_TYPE]) + ''] ('' + CAST([CHARACTER_MAXIMUM_LENGTH] AS VARCHAR(10)) + '') NULL;''
FROM  [' +  @DatabaseName + N'].[INFORMATION_SCHEMA].[COLUMNS] C
WHERE
	[DATA_TYPE] NOT LIKE ''xml'' 
	AND [CHARACTER_MAXIMUM_LENGTH] > 1000
	AND [IS_NULLABLE] = ''NO'' 
	AND [TABLE_NAME] NOT LIKE ''syncobj[_]0x%''
	AND EXISTS (
		SELECT TOP 1 1 -- Skip views
		FROM ' + QUOTENAME(@DatabaseName) + N'.[INFORMATION_SCHEMA].[TABLES] Z
		WHERE Z.[TABLE_TYPE] = ''BASE TABLE''
			AND Z.[TABLE_NAME] = C.[TABLE_NAME] 
			AND Z.[TABLE_SCHEMA] = C.[TABLE_SCHEMA]
	)
	AND NOT EXISTS (
		SELECT TOP 1 1 
		FROM ' + QUOTENAME(DB_NAME()) + N'.[Replication].ProtectedArticleColumns AS PAC
		WHERE
			    PAC.DatabaseName = C.TABLE_CATALOG
			AND PAC.SchemaName = C.TABLE_SCHEMA
			AND PAC.TableName = C.[TABLE_NAME]
			AND PAC.ColumnName = C.COLUMN_NAME
		)
'       ;

        SELECT @SPExec += N'
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| ALLOW NULLs FOR COLUMN''S IN [Replication].ExcludedArticleColumns IsDynamicalyAdded = 0
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
--Added by M.N. 25 Jun 2015 ALLOW NULLs WHERE COLUMNS in ExcludedArticleColumns AND NOT NULL
        
SELECT @SPOutput += ''
ALTER TABLE ['' + [TABLE_SCHEMA] + ''].['' + [TABLE_NAME] + ''] ALTER COLUMN ['' + [COLUMN_NAME] + ''] ['' + UPPER([DATA_TYPE]) + ''] ''
		+ CASE	WHEN [CHARACTER_MAXIMUM_LENGTH] IS NOT NULL
				THEN ''('' +CAST([CHARACTER_MAXIMUM_LENGTH] AS VARCHAR(10)) + '')''
				ELSE ''''
			END + '' NULL;''
	FROM  ' + QUOTENAME(@DatabaseName) + N'.[INFORMATION_SCHEMA].[COLUMNS] C
	WHERE
		[IS_NULLABLE] = ''NO'' 
		AND EXISTS (
			SELECT TOP 1 1 
			FROM ' + QUOTENAME(DB_NAME()) + N'.[Replication].ExcludedArticleColumns EC
			WHERE
				EC.DatabaseName = C.TABLE_CATALOG
				AND EC.SchemaName = C.TABLE_SCHEMA
				AND EC.TableName = C.[TABLE_NAME]
				AND EC.ColumnName = C.COLUMN_NAME
				AND EC.IsDynamicalyAdded = 0
			) 
		AND EXISTS (
			SELECT TOP 1 1 -- Skip views
			FROM ' + QUOTENAME(@DatabaseName) + N'.[INFORMATION_SCHEMA].[TABLES] Z
			WHERE
				Z.[TABLE_TYPE] = ''BASE TABLE''
				AND Z.[TABLE_NAME] = C.[TABLE_NAME] 
				AND Z.[TABLE_SCHEMA] = C.[TABLE_SCHEMA]
		)
'       ;
    END;

    EXEC sys.sp_executesql
        @SPExec
      , N'@SPOutput NVARCHAR(MAX) OUTPUT'
      , @SPOutput = @SPOutput OUTPUT;

    SELECT @SQL += N'
SELECT @SQL = @SQL + N''' + @SPOutput + N'
'';' +  CHAR (10);

    SELECT @SQL += N'
IF @SQL NOT LIKE ''''
BEGIN

	SELECT @SQL = ''
	USE ' + QUOTENAME(@DatabaseName) + N';

	BEGIN TRANSACTION;'' + @SQL + CHAR(10) + ''
	COMMIT TRANSACTION;''

-- Debug
	SELECT '' DON''''T RUN IT! This query WILL BE Executed by the SP!''  
	SELECT @SQL FOR XML PATH('''')
	EXEC SP_EXECUTESQL @SQL

END
ELSE 
BEGIN
	RAISERROR('' Warning! we have nothing to change!'', 10, 1)
END
'   ;

    -- Added by KF 14 Apr 2015: 
    -- Switch Read_committed ==> Read_committed_snapshot
    IF @DatabaseName = 'TradeNetworks'
        SELECT @SQL += N'
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| SET READ_COMMITTED_SNAPSHOT ON
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
USE [master];
ALTER DATABASE  ' + QUOTENAME(@DatabaseName) + N' SET READ_COMMITTED_SNAPSHOT ON;
'       ;

    -- Added by M.N. 29 Oct 2018 Switch the suscriber to SIMPLE recovery
    SELECT @SQL += N'
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| SET THE DB IN SIMPLE RECOVERY 
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
USE [master];
ALTER DATABASE  ' + QUOTENAME(@DatabaseName) + N' SET RECOVERY SIMPLE;
'   ;

    -- Added by M.N. 24 Jun 2015: execute [Security].[FixPermisions] for database (Application and Debug)
    SELECT @SQL += N'
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Fix additional objects per subscriber
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
USE ' + QUOTENAME(DB_NAME()) + N'
EXEC [Replication].[CreateAdditionalObjectsPerSubscriber] @DatabaseName = ''' + @DatabaseName + N''';

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Fix all the permission for the database
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
USE ' + QUOTENAME(DB_NAME()) + N'
EXEC [Security].[FixPermisions] @DropExistingLogins = 0, @DebugOutput = 0, @ApplicationServerLogins = 1, @ForUser = NULL, @ForDataBase = ''' + @DatabaseName + N''', @UseDefaultDomainColum = 1;
EXEC [Security].[FixPermisions] @DropExistingLogins = 0, @DebugOutput = 0, @ApplicationServerLogins = 0, @ForUser = NULL, @ForDataBase = ''' + @DatabaseName + N''', @UseDefaultDomainColum = 1;
'   ;
    SELECT CAST(@SQL AS XML);
END TRY
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Catch Error:
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
BEGIN CATCH
    IF @@TRANCOUNT > 0
        ROLLBACK TRANSACTION;

    DECLARE @ErrorStr NVARCHAR (4000);
    SELECT @ErrorStr = dbo.FlushErrorString_FN ();
    RAISERROR (@ErrorStr, 16, 1);
    RETURN -1;
END CATCH;

GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[Step5_PublisherStart]'
GO
/******************************************************************************************\ 
DB Object		: Stored Procedure
Objec Name		: [Replication].[Step5_PublisherStart]
Purpose			: First step of the transactional replication
Author			: Kostya Fridman
Creation Date	: 18 Nov 2014
-------------------------------------------------------------------------------------------
Demo Call:
	EXEC [Replication].[Step5_PublisherStart] @DatabaseName = N'TradeNetworks', @SubscriberInstanceName = N'SOFQAPROD-REP'

-------------------------------------------------------------------------------------------
Changes History:
================
Author		: Martin Nikolov
Date		: 08 Jul 2015
Description	: added additional input parameter @SubscriberInstanceName
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 30 Jul 2015
Description	: added IsEnabled check
-------------------------------------------------------------------------------------------
Author		: 
Date		: 
Description	: 
\******************************************************************************************/
CREATE PROCEDURE [Replication].[Step5_PublisherStart]
    @DatabaseName           NVARCHAR (256)
  , @SubscriberInstanceName NVARCHAR (256) -- Added by M.N. 08 Jul 2015
AS
SET NOCOUNT ON;

BEGIN TRY

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Server validation
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    IF NOT EXISTS (
                      SELECT TOP 1
                             1
                      FROM   [Replication].Definitions
                      WHERE
                             DatabaseName           = @DatabaseName
                         AND SubscriberInstanceName = @SubscriberInstanceName -- Added by M.N. 08 Jul 2015
                         AND IsEnabled              = 1 -- Added by M.N. 30 Jul 2015
                         AND @@SERVERNAME           = PublisherInstanceName
                  )
    BEGIN
        RAISERROR ('Warning! You are trying to execute SP on wrong server, or with wrong combination DatabaseName/SubscriberInstanceName!
      Please check [PublisherInstanceName], [DatabaseName], [SubscriberInstanceName], [IsEnabled] in [Replication].[Definitions]!', 16, 1);
        RETURN -1;
    END;

    DECLARE
        @Publication      sysname         -- 'ProductionTradenetworksAMS'
      , @Subscriber       sysname         -- 'PRODAMSBI_DB'
      , @Destination_db   sysname         -- 'TradeNetworks'
      , @Backupdevicename NVARCHAR (2000) -- 'R:\TradeNetworks.bak'
      , @job_login        NVARCHAR (256)
      , @job_password     sysname;

    SELECT
        @Publication      = [Replication].GetPublicationName (@DatabaseName, @SubscriberInstanceName)                -- Changed by M.N. 08 Jul 2015, added @SubscriberInstanceName
      , @Backupdevicename = [Replication].GetBackupFileAddressOnSubscriber (@DatabaseName, @SubscriberInstanceName); -- Changed by M.N. 08 Jul 2015, added @SubscriberInstanceName

    SELECT
         @Subscriber     = SubscriberInstanceName
       , @Destination_db = DatabaseName
       , @job_login      = WindowsLoginName
       , @job_password   = WindowsLoginPassword
    FROM [Replication].Definitions
    WHERE
         @DatabaseName          = DatabaseName
     AND SubscriberInstanceName = @SubscriberInstanceName; -- Added by M.N. 08 Jul 2015


    -- DEBUG 
    --SELECT  @Publication   AS [@Publication]    
    --   , @Subscriber   AS [@Subscriber]   
    --   , @Destination_db  AS [@Destination_db]  
    --   , @Backupdevicename  AS [@Backupdevicename] 
    --   , @job_login    AS [@job_login] 
    --   , @job_password   AS [@job_password] 

    DECLARE @SQL NVARCHAR (MAX) = N'
 EXEC ' + QUOTENAME(@DatabaseName) + N'.sys.sp_addsubscription 
	  @publication			= @publication                                                                     
	, @article				= @article                           
	, @subscriber			= @subscriber                        
	, @destination_db		= @destination_db                        
	, @sync_type			= @sync_type           
	, @subscription_type	= @subscription_type   
	, @update_mode			= @update_mode         
	, @backupdevicetype		= @backupdevicetype    
	, @backupdevicename		= @backupdevicename    
	, @subscriber_type		= @subscriber_type ';

    -- DEBUG
    --SELECT @SQL

    --  select @publication                                                                                             
    --  select @subscriber                        
    --  select @destination_db                                   
    --  select @backupdevicename    



    EXEC sys.sp_executesql
        @SQL
      , N'
				 @publication			sysname  
				, @article				sysname  
				, @subscriber			sysname  
				, @destination_db		sysname  
				, @sync_type			nvarchar(255)
				, @subscription_type	nvarchar(4)
				, @update_mode			nvarchar(30)
				, @backupdevicetype		nvarchar(20)
				, @backupdevicename		nvarchar(1000)
				, @subscriber_type		tinyint
			'
      , @publication = @Publication
      , @article = N'all'
      , @subscriber = @Subscriber
      , @destination_db = @Destination_db
      , @subscription_type = N'Push'
      , @sync_type = N'initialize with backup'
      , @update_mode = N'read only'
      , @backupdevicetype = 'Disk'
      , @backupdevicename = @Backupdevicename
      , @subscriber_type = 0;

    --DEBUG
    PRINT 'sp_addsubscription is done!';

    SELECT @SQL = N'EXEC ' + QUOTENAME(@DatabaseName) + N'.sys.sp_addpushsubscription_agent 
				  @publication					= @publication      
				, @subscriber					= @subscriber     
				, @subscriber_db				= @subscriber_db    
				, @subscriber_security_mode		= @subscriber_security_mode  
				, @job_login					= @job_login     
				, @job_password					= @job_password     
				, @frequency_type				= @frequency_type    
				, @frequency_interval			= @frequency_interval   
				, @frequency_relative_interval	= @frequency_relative_interval 
				, @frequency_recurrence_factor	= @frequency_recurrence_factor 
				, @frequency_subday				= @frequency_subday    
				, @frequency_subday_interval	= @frequency_subday_interval 
				, @active_start_time_of_day		= @active_start_time_of_day  
				, @active_end_time_of_day		= @active_end_time_of_day  
				, @active_start_date			= @active_start_date   
				, @active_end_date				= @active_end_date    
				, @dts_package_location			= @dts_package_location   
				, @enabled_for_syncmgr			= @enabled_for_syncmgr ';

    DECLARE @active_start_date INT = (
                                         SELECT REPLACE (CONVERT (VARCHAR (100), CAST(GETDATE () AS DATE), 111), '/', '')
                                     );

    EXEC sys.sp_executesql
        @SQL
      , N'
				  @publication					sysname
				, @subscriber					sysname
				, @subscriber_db				sysname
				, @subscriber_security_mode		smallint  
				, @job_login					nvarchar (256)
				, @job_password					sysname
				, @frequency_type				int
				, @frequency_interval			int
				, @frequency_relative_interval	int
				, @frequency_recurrence_factor	int
				, @frequency_subday				int
				, @frequency_subday_interval	int
				, @active_start_time_of_day		int
				, @active_end_time_of_day		int
				, @active_start_date			int
				, @active_end_date				int
				, @dts_package_location			nvarchar (12)
				, @enabled_for_syncmgr			nvarchar (5)'
      , @publication = @Publication
      , @subscriber = @Subscriber
      , @subscriber_db = @Destination_db
      , @job_login = @job_login
      , @job_password = @job_password
      , @subscriber_security_mode = 1
      , @frequency_type = 64
      , @frequency_interval = 0
      , @frequency_relative_interval = 0
      , @frequency_recurrence_factor = 0
      , @frequency_subday = 0
      , @frequency_subday_interval = 0
      , @active_start_time_of_day = 0
      , @active_end_time_of_day = 235959
      , @active_start_date = @active_start_date
      , @active_end_date = 99991231
      , @enabled_for_syncmgr = N'False'
      , @dts_package_location = N'Distributor';

END TRY
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Catch Error:
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
BEGIN CATCH
    IF @@TRANCOUNT > 0
        ROLLBACK TRANSACTION;

    DECLARE @ErrorStr NVARCHAR (4000);
    SELECT @ErrorStr = dbo.FlushErrorString_FN ();
    RAISERROR (@ErrorStr, 16, 1);
    RETURN -1;
END CATCH;
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[07.PublisherCopyBackupToSubscriber]'
GO
/******************************************************************************************\ 
DB Object		: Stored Procedure
Objec Name		: [Replication].[07.PublisherCopyBackupToSubscriber]
Purpose			: Copy backup file from publisher to subscriber
Author			: Kostya Fridman
Creation Date	: 19 Nov 2014
-------------------------------------------------------------------------------------------
Demo Call:
	EXEC [Replication].[07.PublisherCopyBackupToSubscriber] @DatabaseName = N'TradeNetworks', @SubscriberInstanceName = N'ProdAmsRODB\ROF'

-------------------------------------------------------------------------------------------
Changes History:
================
Author		: Martin Nikolov
Date		: 08 Jul 2015
Description	: added additional input parameter @SubscriberInstanceName
				Additional logick to create the backup only when we process the last publication for the database.
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 30 Jul 2015
Description	: added IsEnabled check
-------------------------------------------------------------------------------------------
Author		: 
Date		: 
Description	: 
\******************************************************************************************/
CREATE PROCEDURE [Replication].[07.PublisherCopyBackupToSubscriber]
    @DatabaseName           NVARCHAR (256)
  , @SubscriberInstanceName NVARCHAR (256) -- Added by M.N. 08 Jul 2015
AS
SET NOCOUNT ON;

BEGIN TRY

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Server validation
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    IF NOT EXISTS (
                      SELECT TOP 1
                             1
                      FROM   [Replication].Definitions
                      WHERE
                             DatabaseName           = @DatabaseName
                         AND SubscriberInstanceName = @SubscriberInstanceName -- Added by M.N. 08 Jul 2015
                         AND IsEnabled              = 1 -- Added by M.N. 30 Jul 2015
                         AND @@SERVERNAME           = PublisherInstanceName
                  )
    BEGIN
        RAISERROR ('Warning! You are trying to execute SP on wrong server, or with wrong combination DatabaseName/SubscriberInstanceName!
      Please check [PublisherInstanceName], [DatabaseName], [SubscriberInstanceName], [IsEnabled] in [Replication].[Definitions]!', 16, 1);
        RETURN -1;
    END;

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Added by M.N. 10 Jul 2015 to cove the backup issue when we have more than 1 subscriber for one database 
| We need to make the backup when we process the last publication
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    IF (
           SELECT COUNT (1)
           FROM   [Replication].Definitions
           WHERE
                  DatabaseName       = @DatabaseName
              AND IsEnabled          = 1 -- Added by M.N. 30 Jul 2015
              AND PublisherProcessed = 0
       ) > 1
    BEGIN
        UPDATE [Replication].Definitions
        SET    PublisherProcessed = 1
        WHERE
               DatabaseName           = @DatabaseName
           AND SubscriberInstanceName = @SubscriberInstanceName
           AND PublisherProcessed     = 0;

        RETURN 0;
    END;
    /* Adeed by M.N. 10 Jul 2015 TO HERE */

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Copy Backup From Publisher To Distributor
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

    DECLARE @SQL NVARCHAR (MAX);
    SELECT @SQL = N'EXEC master.sys.sp_configure ''show advanced options'', 1;
 RECONFIGURE;
 EXEC master.sys.sp_configure ''xp_cmdshell'', 1;
 RECONFIGURE;
 EXEC master.sys.xp_cmdshell ''copy "' + [Replication].GetBackupFileAddressOnPublisher (@DatabaseName, @SubscriberInstanceName) -- Changed by M.N. 08 Jul 2015, added @SubscriberInstanceName
+       N'" "' + SubscriberInstanceBackupFolder + N'" ''
 EXEC master.sys.sp_configure ''xp_cmdshell'', 0;
 RECONFIGURE;
 EXEC master.sys.sp_configure ''show advanced options'',0;
 RECONFIGURE;
'
    FROM   [Replication].Definitions
    WHERE
           DatabaseName                  = @DatabaseName
       AND SubscriberInstanceName        = @SubscriberInstanceName -- Added by M.N. 08 Jul 2015
       AND PublisherInstanceBackupFolder <> SubscriberInstanceBackupFolder;

    -- debug: 
    PRINT @SQL;

    EXEC sys.sp_executesql @SQL;

    UPDATE [Replication].Definitions
    SET    PublisherProcessed = 0
    WHERE
           DatabaseName       = @DatabaseName
       AND PublisherProcessed = 1;

END TRY
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Catch Error:
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
BEGIN CATCH
    IF @@TRANCOUNT > 0
        ROLLBACK TRANSACTION;

    DECLARE @ErrorStr NVARCHAR (4000);
    SELECT @ErrorStr = dbo.FlushErrorString_FN ();
    RAISERROR (@ErrorStr, 16, 1);
    RETURN -1;
END CATCH;
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[Step3_RunPublisherProcess]'
GO
/******************************************************************************************\ 
DB Object		: Stored Procedure
Objec Name		: [Replication].[Step3_RunPublisherProcess]
Purpose			: The general step, which executes all the steps to create and prepare the Publisher
Author			: Ivo Ivanov
Creation Date	: 20 Nov 2014
-------------------------------------------------------------------------------------------
Demo Call:
-- sp_removedbreplication 'tradenetworks'

	EXEC [Replication].[Step3_RunPublisherProcess] @DatabaseName = N'Configuration', @SubscriberInstanceName = N'SOFDBA\DBA'

-------------------------------------------------------------------------------------------
Changes History:
================
Author		: Kostya Fridman
Date		: 21 Nov 2014
Description	: the step "[Replication].[01.AddLoginToAdminGroup]" was inserted 
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 08 Jul 2015
Description	: added additional input parameter @SubscriberInstanceName
-------------------------------------------------------------------------------------------
Author		: Martin Nikolov
Date		: 30 Jul 2015
Description	: added IsEnabled check
-------------------------------------------------------------------------------------------
Author		: 
Date		: 
Description	: 
\******************************************************************************************/
CREATE PROCEDURE [Replication].[Step3_RunPublisherProcess]
    @DatabaseName           NVARCHAR (256)
  , @SubscriberInstanceName NVARCHAR (256) -- Added by M.N. 08 Jul 2015
AS
SET NOCOUNT ON;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Server validation
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
IF NOT EXISTS (
                  SELECT TOP 1
                         1
                  FROM   [Replication].Definitions
                  WHERE
                         DatabaseName           = @DatabaseName
                     AND SubscriberInstanceName = @SubscriberInstanceName -- Added by M.N. 08 Jul 2015
                     AND IsEnabled              = 1 -- Added by M.N. 30 Jul 2015
                     AND @@SERVERNAME           = PublisherInstanceName
              )
BEGIN
    RAISERROR ('Warning! You are trying to execute SP on wrong server, or with wrong combination DatabaseName/SubscriberInstanceName!
      Please check [PublisherInstanceName], [DatabaseName], [SubscriberInstanceName], [IsEnabled] in [Replication].[Definitions]!', 16, 1);
    RETURN -1;
END;

DECLARE @ReturnCode INT;

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Execute Publisher step 1: AllowReplication
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

PRINT 'Run step 1: ''AllowReplication''';

EXEC @ReturnCode = [Replication].[02.PublisherAllowReplication]
    @DatabaseName = @DatabaseName
  , @SubscriberInstanceName = @SubscriberInstanceName; -- Added by M.N. 08 Jul 2015

IF (@ReturnCode <> 0)
BEGIN
    RAISERROR ('The execution of step 1 ''AllowReplication'' failed!', 16, 1);
    RETURN -1;
END;

PRINT 'Step 1 ''AllowReplication'' done!';


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Execute Publisher step 2: AddPublication
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

PRINT 'Run step 2: ''AddPublication''';

EXEC @ReturnCode = [Replication].[03.PublisherAddPublication]
    @DatabaseName = @DatabaseName
  , @SubscriberInstanceName = @SubscriberInstanceName; -- Added by M.N. 08 Jul 2015

IF (@ReturnCode <> 0)
BEGIN
    RAISERROR ('The execution of step 2 ''AddPublication'' failed!', 16, 1);
    RETURN -2;
END;

PRINT 'Step 2 ''AddPublication'' done!';


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Execute Publisher step 3: Add LoginToAdminGroup
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

PRINT 'Run step 3: ''AddLoginToAdminGroup''';
EXEC @ReturnCode = [Replication].[01.AddLoginToAdminGroup]
    @DatabaseName = @DatabaseName
  , @SubscriberInstanceName = @SubscriberInstanceName -- Added by M.N. 08 Jul 2015
  , @RunFlag = 1
  , @PrintFlag = 0;

IF (@ReturnCode <> 0)
BEGIN
    RAISERROR ('The execution of step 3 ''AddLoginToAdminGroup'' failed!', 16, 1);
    RETURN -3;
END;

PRINT 'Step 3 ''AddLoginToAdminGroup'' done!';


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Execute Publisher step 4: AddArticles
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

PRINT 'Run step 4: ''AddArticles''';

EXEC @ReturnCode = [Replication].[04.PublisherAddArticles]
    @DatabaseName = @DatabaseName
  , @SubscriberInstanceName = @SubscriberInstanceName; -- Added by M.N. 08 Jul 2015

IF (@ReturnCode <> 0)
BEGIN
    RAISERROR ('The execution of step 4 ''AddArticles'' failed!', 16, 1);
    RETURN -4;
END;

PRINT 'Step 4 ''AddArticles'' done!';


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Execute Publisher step 5: DropSpecificColumns
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

PRINT 'Run step 5: ''DropSpecificColumns''';

EXEC @ReturnCode = [Replication].[05.PublisherDropSpecificColumns]
    @DatabaseName = @DatabaseName
  , @SubscriberInstanceName = @SubscriberInstanceName; -- Added by M.N. 08 Jul 2015

IF (@ReturnCode <> 0)
BEGIN
    RAISERROR ('The execution of step 5 ''DropSpecificColumns'' failed!', 16, 1);
    RETURN -5;
END;

PRINT 'Step 5 ''DropSpecificColumns'' done!';


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Execute Publisher step 6: RunBackup
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

PRINT 'Run step 6: ''RunBackup''';

EXEC @ReturnCode = [Replication].[06.PublisherRunBackup]
    @DatabaseName = @DatabaseName
  , @SubscriberInstanceName = @SubscriberInstanceName; -- Added by M.N. 08 Jul 2015

IF (@ReturnCode <> 0)
BEGIN
    RAISERROR ('The execution of step 6 ''RunBackup'' failed!', 16, 1);
    RETURN -6;
END;

PRINT 'Step 6 ''RunBackup'' done!';


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Execute Publisher step 7: CopyBackupToSubscriber
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

PRINT 'Run step 7: ''CopyBackupToSubscriber''';

EXEC @ReturnCode = [Replication].[07.PublisherCopyBackupToSubscriber]
    @DatabaseName = @DatabaseName
  , @SubscriberInstanceName = @SubscriberInstanceName; -- Added by M.N. 08 Jul 2015

IF (@ReturnCode <> 0)
BEGIN
    RAISERROR ('The execution of step 7 ''CopyBackupToSubscriber'' failed!', 16, 1);
    RETURN -7;
END;

PRINT 'Step 7 ''CopyBackupToSubscriber'' done!';
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[Step2_ArticlesVerification]'
GO
/******************************************************************************************\ 
DB Object		: Stored Procedure
Objec Name		: [Replication].[Step2_ArticlesVerification]
Purpose			: Compare table [Replication].[Articles] with list of tables and get missing objects
Author			: Kostya Fridman
Creation Date	: 21 Nov 2014
-------------------------------------------------------------------------------------------
Demo Call: 

	EXEC [Replication].[Step2_ArticlesVerification] @DatabaseName = N'Configuration', @SubscriberInstanceName = 'SOFDBA\DBA'

-------------------------------------------------------------------------------------------
Changes History:
================
Author		: Martin Nikolov
Date		: 30 Jul 2015
Description	: added IsEnabled check
-------------------------------------------------------------------------------------------
Author		: Gergana Bivolarska
Date		: 14 Jan 2019
Approved By : Martin Nikolov
Description	: #87526 - Different articles per subscriber
-------------------------------------------------------------------------------------------
Author		: Gergana Bivolarska
Date		: 12 Feb 2019
Approved By : Martin Nikolov
Description	: #88403 
-------------------------------------------------------------------------------------------
Changed By  : Martin Nikolov
Changed Date: 09 Mar 2020
Approved By : Gergana Bivolarska
Description : Chaned the [INFORMATION_SCHEMA] to "sys." tables. Removed harcoded expludes.
-------------------------------------------------------------------------------------------
Changed By  : Martin Nikolov
Changed Date: 23 Jan 2022
Approved By : 
Description : Added output of Schema and Table. Added option to EXEC [Replication].AddSingleArticle
-------------------------------------------------------------------------------------------
Changed By  : 
Changed Date: 
Approved By : 
Description : 
\******************************************************************************************/
CREATE PROCEDURE [Replication].[Step2_ArticlesVerification]
    @DatabaseName           NVARCHAR (256)
  , @SubscriberInstanceName NVARCHAR (256) --Added by G.B. 11 Jan 2019
AS
SET NOCOUNT ON;

BEGIN TRY

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Server validation
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    IF NOT EXISTS (
                      SELECT TOP 1
                             1
                      FROM   [Replication].Definitions
                      WHERE
                             DatabaseName = @DatabaseName
                         AND IsEnabled    = 1 -- Added by M.N. 30 Jul 2015
                         AND @@SERVERNAME = PublisherInstanceName
                  )
    BEGIN
        RAISERROR ('Warning! You are trying to execute SP on wrong server, or with wrong combination DatabaseName/SubscriberInstanceName!
      Please check [PublisherInstanceName], [DatabaseName], [SubscriberInstanceName], [IsEnabled] in [Replication].[Definitions]!', 16, 1);
        RETURN -1;
    END;

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Get Missing articles
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    CREATE TABLE #ListOfMissingArticles (
        ArticleValues     NVARCHAR (4000) NOT NULL
      , ArticleSchemaName NVARCHAR (256)  NOT NULL
      , ArticleTableName  NVARCHAR (256)  NOT NULL
    );

    DECLARE @SQL NVARCHAR (MAX) = N'USE ' + QUOTENAME(@DatabaseName) + N'
SELECT
    ''('''''                      + @DatabaseName + N''''', '''''' + s.name + '''''', '''''' + t.name  + '''''', ''''' + @SubscriberInstanceName + N''''')''
  , s.name
  , t.name
FROM
           sys.schemas                          AS s
INNER JOIN sys.tables                           AS t
        ON s.schema_id              = t.schema_id
INNER JOIN sys.indexes                          AS i
        ON t.object_id              = i.object_id
       AND i.type                   = 1
LEFT JOIN  ' + QUOTENAME(DB_NAME()) + N'.[Replication].Articles AS B
       ON  B.SchemaName             = s.name
       AND B.TableName              = t.name
       AND B.DatabaseName           = DB_NAME ()
       AND B.SubscriberInstanceName = ''' + @SubscriberInstanceName + N''' --Added by G.B. 14 Jan 2019
WHERE
           B.TableName IS NULL
       AND t.is_ms_shipped = 0
       AND NOT EXISTS (
                          SELECT TOP 1
                                 1
                          FROM   ' + QUOTENAME(DB_NAME()) + N'.[Replication].ExcludedArticleTables AS ert
                          WHERE
                                 ert.SchemaName             = s.name
                             AND ert.TableName              = t.name
                             AND ert.DatabaseName           = DB_NAME ()
                             AND ert.SubscriberInstanceName = ''' + @SubscriberInstanceName + N'''
                      )
ORDER BY
           s.name
         , t.name;';
    -- Debug
    -- PRINT @SQL

    INSERT INTO #ListOfMissingArticles (
                                           ArticleValues
                                         , ArticleSchemaName
                                         , ArticleTableName
                                       )
    EXEC sys.sp_executesql @SQL;
    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Print Results
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    IF EXISTS (
                  SELECT TOP 1
                         1
                  FROM   #ListOfMissingArticles
              )
    BEGIN
        SELECT '--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
--!!! WARNING! Please review following articles. You can fix it on Publisher now.  !!!
--!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!';

        SELECT
             ArticleSchemaName
           , ArticleTableName
           , 'INSERT INTO ' + QUOTENAME(DB_NAME()) + N'.[Replication].[Articles] ([DatabaseName], [SchemaName], [TableName], [SubscriberInstanceName]) VALUES ' + RTRIM (ArticleValues) + ';'              AS IncludeArticles
           , 'INSERT INTO ' + QUOTENAME(DB_NAME()) + N'.[Replication].[ExcludedArticleTables] ([DatabaseName], [SchemaName], [TableName], [SubscriberInstanceName]) VALUES ' + RTRIM (ArticleValues) + ';' AS ExcludeArticles
           , 'EXEC ' + QUOTENAME(DB_NAME()) + N'.[Replication].AddSingleArticle
    @DatabaseName = N''' + @DatabaseName + '''
  , @SubscriberInstanceName = N''' + @SubscriberInstanceName + '''
  , @SchemaName = N''' + ArticleSchemaName + '''
  , @TableName = N''' + ArticleTableName + '''
  , @Debug = 0
'                                                                                                                                                                                            AS ExecAddSingleArticle
        FROM #ListOfMissingArticles
        ORDER BY
             ArticleSchemaName
           , ArticleTableName;
    END;
    ELSE
        SELECT 'OK!';

END TRY
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Catch Error:
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
BEGIN CATCH
    IF @@TRANCOUNT > 0
        ROLLBACK TRANSACTION;

    DECLARE @ErrorStr NVARCHAR (4000);
    SELECT @ErrorStr = dbo.FlushErrorString_FN ();
    RAISERROR (@ErrorStr, 16, 1);
    RETURN -1;
END CATCH;
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[AddSingleArticle]'
GO
/******************************************************************************************\  
Project	    : Replication
Object Name : AddSingleArticle
Object Type : Stored Procedure
Created By  : Gergana Bivolarska
Created Date: 11 Jan 2019
Approved By : Martin Nikolov
Approved By : 
Description : Add a single table to an existing replication
-------------------------------------------------------------------------------------------
Demo Call:

     EXEC [Replication].AddSingleArticle
        @DatabaseName           = 'Configuration'
      , @SubscriberInstanceName = 'SOFDBA\DBA'
      , @SchemaName             = 'Alerts'
      , @TableName              = 'Configurations'

-------------------------------------------------------------------------------------------
Changes History:
================
Changed By  : Martin Nikolov
Changed Date: 23 Jan 2022
Approved By : 
Description : Remove the check if the table exists on the targer.
-------------------------------------------------------------------------------------------
Changed By  : Martin Nikolov
Changed Date: 27 May 2022
Approved By :
Description : Changed the add_article call to be the same as in [Replication].[04.PublisherAddArticles]
-------------------------------------------------------------------------------------------
Changed By  : 
Changed Date: 
Approved By : 
Description : 
\******************************************************************************************/
CREATE PROCEDURE [Replication].[AddSingleArticle]
    @DatabaseName           NVARCHAR (256)
  , @SubscriberInstanceName NVARCHAR (256)
  , @SchemaName             NVARCHAR (256)
  , @TableName              NVARCHAR (256)
  , @Debug                  BIT = 0
AS
SET NOCOUNT ON;
BEGIN TRY

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Server validation
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    IF NOT EXISTS (
                      SELECT TOP 1
                             1
                      FROM   [Replication].Definitions
                      WHERE
                             DatabaseName           = @DatabaseName
                         AND SubscriberInstanceName = @SubscriberInstanceName
                         AND IsEnabled              = 1
                         AND @@SERVERNAME           = PublisherInstanceName
                  )
    BEGIN
        RAISERROR ('Warning! You are trying to execute SP on wrong server, or with wrong combination DatabaseName/SubscriberInstanceName!
      Please check [PublisherInstanceName], [DatabaseName], [SubscriberInstanceName], [IsEnabled] in [Replication].[Definitions]!', 16, 1);
        RETURN -1;
    END;

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Declare variables
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    DECLARE
        @publication                   sysname
      , @article                       sysname
      , @source_table                  NVARCHAR (386)
      , @destination_table             sysname
      , @vertical_partition            NCHAR (5)
      , @type                          sysname
      , @filter                        NVARCHAR (386)
      , @sync_object                   NVARCHAR (386)
      , @ins_cmd                       NVARCHAR (255)
      , @del_cmd                       NVARCHAR (255)
      , @upd_cmd                       NVARCHAR (255)
      , @creation_script               NVARCHAR (255)
      , @description                   NVARCHAR (255)
      , @pre_creation_cmd              NVARCHAR (10)
      , @schema_option                 VARBINARY (4)
      , @destination_owner             sysname
      , @status                        TINYINT      --- Specifies if the article is active and additional options for how changes are propagated. status is tinyint, and can be the | (Bitwise OR) product of one or more of these values.
      , @source_owner                  sysname
      , @sync_object_owner             sysname
      , @filter_owner                  sysname
      , @source_object                 sysname
      , @artid                         INT
      , @auto_identity_range           NVARCHAR (5)
      , @pub_identity_range            BIGINT
      , @identity_range                BIGINT
      , @threshold                     INT
      , @force_invalidate_snapshot     BIT          -- Acknowledges that the action taken by this stored procedure may invalidate an existing snapshot. force_invalidate_snapshot is a bit, with a default of 0.
      , @use_default_datatypes         BIT          -- Is whether the default column data type mappings are used when publishing an article from an Oracle Publisher. use_default_datatypes is bit, with a default of 1.
      , @identityrangemanagementoption NVARCHAR (10)
      , @publisher                     sysname
      , @fire_triggers_on_snapshot     NVARCHAR (5) --Is if replicated user triggers are executed when the initial snapshot is applied. fire_triggers_on_snapshot is nvarchar(5), with a default of FALSE.
      , @SQL                           NVARCHAR (MAX) = N'';

    SELECT
        @publication                   = [Replication].GetPublicationName (@DatabaseName, @SubscriberInstanceName) -- Changed by M.N. 08 Jul 2015, added @SubscriberInstanceName
      , @type                          = N'logbased'
      , @description                   = NULL
      , @creation_script               = NULL
      , @pre_creation_cmd              = N'drop'
      , @schema_option                 = 0x000000000803509F
      , @identityrangemanagementoption = N'manual'
      , @vertical_partition            = N'false'
      , @ins_cmd                       = N'SQL'
      , @del_cmd                       = N'SQL'
      , @upd_cmd                       = N'SQL'
      , @status                        = 24;                                                                       -- Added by M.N. 28 Jun 2015

    SELECT
        @article           = @SchemaName + '.' + @TableName
      , @source_owner      = @SchemaName
      , @source_object     = @TableName
      , @destination_table = @TableName
      , @destination_owner = @SchemaName;

    IF OBJECT_ID (@DatabaseName + '.' + 'dbo.syspublications') IS NOT NULL
   AND OBJECT_ID (@DatabaseName + '.' + 'dbo.syssubscriptions') IS NOT NULL
   AND OBJECT_ID (@DatabaseName + '.' + 'dbo.sysarticles') IS NOT NULL
    BEGIN

        IF OBJECT_ID (@DatabaseName + '.' + @SchemaName + '.' + @TableName) IS NULL
        BEGIN
            RAISERROR ('Warning! The table does not exist on the publisher machine!
                    Please create the table on the publisher!', 16, 1);
            RETURN -1;
        END;

        SELECT @SQL = N'
        /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
        | Add article to the publication 
        \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
		USE ' + @DatabaseName + N'
        IF OBJECT_ID (''' + @DatabaseName + N'.' + @SchemaName + N'.' + @TableName + N''') IS NOT NULL
        AND NOT EXISTS (
                          SELECT    TOP 1
                                    1
                          FROM
                                    dbo.syssubscriptions AS sub
                         INNER JOIN sys.servers          AS serv
                                 ON serv.server_id = sub.srvid
                         INNER JOIN dbo.sysarticles      AS art
                                 ON art.artid      = sub.artid
                         INNER JOIN dbo.syspublications  AS pub
                                 ON pub.pubid      = art.pubid
                          WHERE
                                    OBJECT_NAME (art.objid)        = ''' + @TableName + N'''
                                AND OBJECT_SCHEMA_NAME (art.objid) = ''' + @SchemaName + N'''
                                AND serv.name                      = ''' + @SubscriberInstanceName + N'''
                      )
        BEGIN
'       ;
        SELECT @SQL += N'
    EXEC ' + QUOTENAME(@DatabaseName) + N'.sys.sp_addarticle
          @publication                      = @publication
        , @article                          = @article
        , @source_table                     = @source_table
        , @destination_table                = @destination_table
        , @vertical_partition               = @vertical_partition
        , @type                             = @type
        , @filter                           = @filter
        , @sync_object                      = @sync_object
        , @ins_cmd                          = @ins_cmd
        , @del_cmd                          = @del_cmd
        , @upd_cmd                          = @upd_cmd
        , @creation_script                  = @creation_script
        , @description                      = @description
        , @pre_creation_cmd                 = @pre_creation_cmd
        , @schema_option                    = @schema_option
        , @destination_owner                = @destination_owner
        , @status                           = @status
        , @source_owner                     = @source_owner
        , @sync_object_owner                = @sync_object_owner
        , @filter_owner                     = @filter_owner
        , @source_object                    = @source_object
        , @artid                            = @artid
        , @auto_identity_range              = @auto_identity_range
        , @pub_identity_range               = @pub_identity_range
        , @identity_range                   = @identity_range
        , @threshold                        = @threshold
        , @identityrangemanagementoption    = @identityrangemanagementoption
        , @publisher                        = @publisher
END'    ;

        IF @Debug = 1
        BEGIN
            PRINT @SQL;
        END;

        EXEC sys.sp_executesql
            @SQL
          , N'
    @publication                      sysname
  , @article                          sysname
  , @source_table                     nvarchar  (386)
  , @destination_table                sysname
  , @vertical_partition               nchar     (5)
  , @type                             sysname
  , @filter                           nvarchar  (386)
  , @sync_object                      nvarchar  (386)
  , @ins_cmd                          nvarchar  (255)
  , @del_cmd                          nvarchar  (255)
  , @upd_cmd                          nvarchar  (255)
  , @creation_script                  nvarchar  (255)
  , @description                      nvarchar  (255)
  , @pre_creation_cmd                 nvarchar  (10)
  , @schema_option                    varbinary (4)
  , @destination_owner                sysname
  , @status                           tinyint
  , @source_owner                     sysname
  , @sync_object_owner                sysname
  , @filter_owner                     sysname
  , @source_object                    sysname
  , @artid                            int
  , @auto_identity_range              nvarchar  (5)
  , @pub_identity_range               bigint
  , @identity_range                   bigint
  , @threshold                        int
  , @identityrangemanagementoption    nvarchar  (10)
  , @publisher                        sysname
'
          , @publication = @publication
          , @article = @article
          , @source_table = @source_table
          , @destination_table = @destination_table
          , @vertical_partition = @vertical_partition
          , @type = @type
          , @filter = @filter
          , @sync_object = @sync_object
          , @ins_cmd = @ins_cmd
          , @del_cmd = @del_cmd
          , @upd_cmd = @upd_cmd
          , @creation_script = @creation_script
          , @description = @description
          , @pre_creation_cmd = @pre_creation_cmd
          , @schema_option = @schema_option
          , @destination_owner = @destination_owner
          , @status = @status -- Included by M.N. 28 Jun 2015
          , @source_owner = @source_owner
          , @sync_object_owner = @sync_object_owner
          , @filter_owner = @filter_owner
          , @source_object = @source_object
          , @artid = @artid
          , @auto_identity_range = @auto_identity_range
          , @pub_identity_range = @pub_identity_range
          , @identity_range = @identity_range
          , @threshold = @threshold
          , @identityrangemanagementoption = @identityrangemanagementoption
          , @publisher = @publisher;

        SELECT @SQL = N'
    EXEC ' + QUOTENAME(@DatabaseName) + N'.sys.sp_refreshsubscriptions
          @publication                      = @publication';

        IF @Debug = 1
        BEGIN
            PRINT @SQL;
        END;

        EXEC sys.sp_executesql
            @SQL
          , N'
    @publication                      sysname'
          , @publication = @publication;

        SELECT @SQL = N'';
        SELECT @SQL = N'IF NOT EXISTS (
                          SELECT TOP 1
                                 1
                          FROM   ' + QUOTENAME(DB_NAME()) + N'.[Replication].Articles
                          WHERE
                                 DatabaseName           = ''' + @DatabaseName + N'''
                             AND SchemaName             = ''' + @SchemaName + N'''
                             AND TableName              = ''' + @TableName + N'''
                             AND SubscriberInstanceName = ''' + @SubscriberInstanceName + N'''
                      )
        BEGIN
            INSERT INTO ' + QUOTENAME(DB_NAME()) + N'.[Replication].Articles (
                                                                 DatabaseName
                                                               , SchemaName
                                                               , TableName
                                                               , SubscriberInstanceName
                                                             )
            VALUES (
                       ''' + @DatabaseName + N''', ''' + @SchemaName + N''', ''' + @TableName + N''', ''' + @SubscriberInstanceName + N'''
                   );
        END;';
        IF @Debug = 1
        BEGIN
            PRINT @SQL;
        END;
        EXEC sys.sp_executesql @SQL;

    END;

--    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
--    | Create table on the subscriber if does not exist
--    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
--    SELECT @SQL = N'';

--    SELECT @SQL = N'
--DECLARE @SQL NVARCHAR (MAX) = N''''
--SELECT
--      i.name AS CONSTRAINT_NAME
--    , x.COLUMN_NAME
--INTO #InformationPrimaryKey
--FROM       ' + @DatabaseName + N'.sys.schemas       AS s
--INNER JOIN ' + @DatabaseName + N'.sys.tables        AS t
--        ON t.schema_id  = s.schema_id
--INNER JOIN ' + @DatabaseName + N'.sys.indexes       AS i
--        ON i.object_id  = t.object_id
--CROSS APPLY (SELECT STUFF ((
--    SELECT N'', '' + c.name
--    FROM
--            ' + @DatabaseName + N'.sys.columns       AS c
--    INNER JOIN ' + @DatabaseName + N'.sys.index_columns AS ic
--            ON c.object_id = ic.object_id
--        AND c.column_id = ic.column_id
--    WHERE
--            ic.object_id = t.object_id
--        AND ic.index_id  = i.index_id
--    FOR XML PATH ('''')
--), 1, 2, N'''') AS COLUMN_NAME) AS X
--WHERE
--    i.is_primary_key = 1
--AND t.name           = ''' + @TableName + N'''
--AND s.name           = ''' + @SchemaName + N''';

--IF NOT EXISTS (
--SELECT TOP 1 1
--FROM PRODAMSROF.' + @DatabaseName + N'.sys.tables AS t
--    INNER JOIN ' + @DatabaseName + N'.sys.schemas AS s 
--            ON s.schema_id = t.schema_id
--WHERE
--    t.name = ''' + @TableName + N'''
--AND s.name = ''' + @SchemaName + N'''
--)
--BEGIN
--    SELECT @SQL = N''EXECUTE(''''CREATE TABLE ' + QUOTENAME(@DatabaseName) + N'.[' + @SchemaName + N'].[' + @TableName + N'] (''' + CHAR (10) + N' + STUFF ((
--               SELECT CHAR (10) + N'', ''
--                    + c.COLUMN_NAME + N'' ''
--                    + c.DATA_TYPE + N'' ''
--                    + CASE WHEN c.CHARACTER_MAXIMUM_LENGTH = -1
--                            AND c.DATA_TYPE <> ''xml''
--                            THEN N''(max)''
--                            WHEN c.CHARACTER_MAXIMUM_LENGTH = -1
--                            AND c.DATA_TYPE = ''xml''
--                            THEN N''''
--                            ELSE ISNULL (N''('' + CAST(c.CHARACTER_MAXIMUM_LENGTH AS NVARCHAR (10)) + '')'', '''')
--                        END + N''''
--                    + CASE WHEN c.DATA_TYPE = ''decimal''
--                            THEN ISNULL (N''('' + CAST(c.NUMERIC_PRECISION AS NVARCHAR (10)) + '','' + CAST(c.NUMERIC_SCALE AS NVARCHAR (10)) + '')'', '''')
--                            ELSE N''''
--                        END + N'''' 
--                    + CASE WHEN c.DATA_TYPE = ''datetime2''
--                            THEN ISNULL (N''('' + CAST(c.DATETIME_PRECISION AS NVARCHAR (10)) + '')'', '''')
--                            ELSE N''''
--                        END + N''''
--                    + CASE WHEN c.DATA_TYPE = ''datetimeoffset''
--                            THEN ISNULL (N''('' + CAST(c.DATETIME_PRECISION AS NVARCHAR (10)) + '')'', '''')
--                            ELSE N''''
--                        END + N''''
--                    + CASE WHEN c.IS_NULLABLE = ''NO'' THEN N'' NOT NULL '' ELSE N'' NULL '' END + N''''
--                    + CASE WHEN COLUMNPROPERTY (OBJECT_ID (c.TABLE_SCHEMA + ''.'' + c.TABLE_NAME), c.COLUMN_NAME, ''IsIdentity'') = 1
--                            THEN N''IDENTITY ('' + CAST(IDENT_SEED (c.TABLE_SCHEMA + N''.'' + c.TABLE_NAME) AS NVARCHAR (10)) + N'','' + CAST(IDENT_INCR (c.TABLE_SCHEMA + N''.'' + c.TABLE_NAME) AS NVARCHAR (10)) + N'') NOT FOR REPLICATION''
--                            ELSE N''''
--                        END
--               FROM   ' + @DatabaseName + N'.INFORMATION_SCHEMA.COLUMNS AS c
--               WHERE
--                   c.TABLE_SCHEMA = ''' + @SchemaName + N'''
--               AND c.TABLE_NAME   = ''' + @TableName + N'''
--               FOR XML PATH ('''')
--            ), 1, 2, '''') + N'')''
--    + N'''''') AT PRODAMSROF''

--    IF @Debug = 1
--    BEGIN
--        PRINT @SQL;
--    END

--    EXEC sys.sp_executesql @SQL

--    SELECT @SQL =  N''
--    EXECUTE(''''ALTER TABLE ' + QUOTENAME(@DatabaseName) + N'.[' + @SchemaName + N'].[' + @TableName + N'] ADD CONSTRAINT '' + CONSTRAINT_NAME + '' PRIMARY KEY CLUSTERED ('' + COLUMN_NAME + N'')'''') AT PRODAMSROF'' -- LINKED SERVER NAME !!!
--    FROM   #InformationPrimaryKey;

--    IF @Debug = 1
--    BEGIN
--        PRINT @SQL;
--    END

--    EXEC sys.sp_executesql @SQL;
--END';
--    IF @Debug = 1
--    BEGIN
--        PRINT @SQL;
--    END;

--    EXEC sys.sp_executesql @SQL, N'@Debug BIT', @Debug = @Debug;

END TRY
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Catch Error:
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
BEGIN CATCH
    IF @@TRANCOUNT > 0
        ROLLBACK TRANSACTION;

    DECLARE @ErrorStr NVARCHAR (4000);
    SELECT @ErrorStr = dbo.FlushErrorString_FN ();
    RAISERROR (@ErrorStr, 16, 1);
    RETURN -1;
END CATCH;
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[RecreateObjectsAndEnableTriggers]'
GO
/******************************************************************************************\ 
DB Object		: Stored Procedure
Objec Name		: [Replication].[RecreateObjectsAndEnableTriggers]
Purpose			: #87654 - Recreate all FK, CK and enable all triggers
Author			: Gergana Bivolarska
Creation Date	: 15 Jan 2019
-------------------------------------------------------------------------------------------
Demo Call:
    
    EXEC [Replication].[RecreateObjectsAndEnableTriggers] @DatabaseName = 'Configuration'

-------------------------------------------------------------------------------------------
Changes History:
================ 
Author		: 
Date		: 
Description	: 
-------------------------------------------------------------------------------------------
\******************************************************************************************/
CREATE PROCEDURE [Replication].[RecreateObjectsAndEnableTriggers]
    @DatabaseName NVARCHAR (256)
AS
SET NOCOUNT ON;

BEGIN TRY
    DECLARE @SQL NVARCHAR (MAX) = N'';

    IF NOT EXISTS (
                      SELECT TOP 1
                             1
                      FROM   dbo.syssubscriptions AS sub
                      WHERE  sub.dest_db = @DatabaseName
                  )
    BEGIN
        RAISERROR ('Warning! This database is not replicated!', 16, 1);
        RETURN -1;
    END;

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Recreate all FKs and CK on specific database
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    SELECT @SQL = N'SELECT STUFF ((
                  SELECT
                        ''''
                      ; '''' + CreateScript
                  FROM  ' + QUOTENAME(DB_NAME()) + N'.DR.RecreateObjects
                  WHERE DatabaseName = ''' + @DatabaseName + N''' FOR XML PATH ('''')
              ), 1, 0, ''''
             ) AS CreatedScript';

    PRINT @SQL;
    EXEC sys.sp_executesql @SQL;

    /*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
    | Enable all triggers
    \*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
    EXEC sys.sp_MSforeachtable 'ALTER TABLE ? ENABLE TRIGGER all';

END TRY
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\ 
| Catch Error:
\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
BEGIN CATCH
    IF @@TRANCOUNT > 0
        ROLLBACK TRANSACTION;

    DECLARE @ErrorStr NVARCHAR (4000);
    SELECT @ErrorStr = dbo.FlushErrorString_FN ();
    RAISERROR (@ErrorStr, 16, 1);
    RETURN -1;
END CATCH;
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating [Replication].[ProtectedArticleColumns]'
GO
CREATE TABLE [Replication].[ProtectedArticleColumns]
(
[DatabaseName] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[SchemaName] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[TableName] [varchar] (256) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
[ColumnName] [varchar] (256) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
)
WITH
(
DATA_COMPRESSION = PAGE
)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Creating primary key [PK_ProtectedArticleColumns] on [Replication].[ProtectedArticleColumns]'
GO
ALTER TABLE [Replication].[ProtectedArticleColumns] ADD CONSTRAINT [PK_ProtectedArticleColumns] PRIMARY KEY CLUSTERED ([DatabaseName], [SchemaName], [TableName], [ColumnName]) WITH (DATA_COMPRESSION = PAGE)
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding foreign keys to [Replication].[AdditionalObjectsPerSubscriber]'
GO
ALTER TABLE [Replication].[AdditionalObjectsPerSubscriber] ADD CONSTRAINT [FK_AdditionalObjectsPerSubscriberObjectType_AdditionalObjectsPerSubscriberTypesObjectType] FOREIGN KEY ([ObjectType]) REFERENCES [Replication].[AdditionalObjectsPerSubscriberTypes] ([ObjectType])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding foreign keys to [Replication].[Articles]'
GO
ALTER TABLE [Replication].[Articles] ADD CONSTRAINT [FK_ReplicationArticlesDatabaseName_ReplicationDatabasesDatabaseName] FOREIGN KEY ([DatabaseName]) REFERENCES [Replication].[Databases] ([DatabaseName])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding foreign keys to [Replication].[Definitions]'
GO
ALTER TABLE [Replication].[Definitions] ADD CONSTRAINT [FK_ReplicationDefinitionsDatabaseName_ReplicationDatabasesDatabaseName] FOREIGN KEY ([DatabaseName]) REFERENCES [Replication].[Databases] ([DatabaseName])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding foreign keys to [Replication].[ExcludedArticleColumns]'
GO
ALTER TABLE [Replication].[ExcludedArticleColumns] ADD CONSTRAINT [FK_ExcludedArticleColumnsDatabaseName_DatabasesDatabaseName] FOREIGN KEY ([DatabaseName]) REFERENCES [Replication].[Databases] ([DatabaseName])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
PRINT N'Adding foreign keys to [Replication].[ExcludedArticleTables]'
GO
ALTER TABLE [Replication].[ExcludedArticleTables] ADD CONSTRAINT [FK_ExcludedArticleTablesDatabaseName_DatabasesDatabaseName] FOREIGN KEY ([DatabaseName]) REFERENCES [Replication].[Databases] ([DatabaseName])
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
COMMIT TRANSACTION
GO
IF @@ERROR <> 0 SET NOEXEC ON
GO
-- This statement writes to the SQL Server Log so SQL Monitor can show this deployment.
IF HAS_PERMS_BY_NAME(N'sys.xp_logevent', N'OBJECT', N'EXECUTE') = 1
BEGIN
    DECLARE @databaseName AS nvarchar(2048), @eventMessage AS nvarchar(2048)
    SET @databaseName = REPLACE(REPLACE(DB_NAME(), N'\', N'\\'), N'"', N'\"')
    SET @eventMessage = N'Redgate SQL Compare: { "deployment": { "description": "Redgate SQL Compare deployed to ' + @databaseName + N'", "database": "' + @databaseName + N'" }}'
    EXECUTE sys.xp_logevent 55000, @eventMessage
END
GO
DECLARE @Success AS BIT
SET @Success = 1
SET NOEXEC OFF
IF (@Success = 1) PRINT 'The database update succeeded'
ELSE BEGIN
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	PRINT 'The database update failed'
END
GO
