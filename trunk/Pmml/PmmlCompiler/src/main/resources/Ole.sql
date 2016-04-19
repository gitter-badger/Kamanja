-- drop schema Ole cascade ;
create schema Ole;

create table Ole.Model
(
    Mid        serial    not null, -- Uniqe id Generates automatically
	Name       varchar   not null, -- Model Name
	IsActive   bool      not null,
	CreateTime TimeStamp not null default current_timestamp,
	ModifyTime TimeStamp,
    CONSTRAINT Ole_Model_Pkey PRIMARY KEY (Name)
)WITHOUT OIDS;

create table Ole.ModelVersion
(
    Vid             serial    not null, -- Uniqe id Generates automatically
    MdlId           int8      not null, -- from Ole.Model.Mid
	MdlVersion      varchar   not null, -- Model Version
	MdlPmml         text      not null, -- Model PMML
	JarPath         varchar   not null, -- Model Jar full path -- Get all Model Classes from jar
	OutputPolicy    int       not null, -- 1 for every message, 2 for only state change (other than 1 or 2, we consider other values as 2)
	IsActive        bool      not null,
	CreateTime      TimeStamp not null default current_timestamp,
	ModifyTime      TimeStamp,
    CONSTRAINT Ole_ModelVersion_Pkey PRIMARY KEY (MdlId, MdlVersion)
    -- , CONSTRAINT Ole_ModelVersion_Model_Fkey  FOREIGN KEY (MdlId) REFERENCES Ole.Model(Mid)
)WITHOUT OIDS;

create table Ole.CustomerModel
(
    Cid                serial    not null, -- Uniqe id Generates automatically
    MdlId              int8      not null, -- from Ole.Model.Mid
	MdlVersion         varchar   not null, -- Model Version
	CustomerName       varchar   not null, -- Curstomer Name to run this model. Need to see how do we get customer information from message. -- For now we take 'all' as name
	InputQueuesTopics  varchar[],          -- Topics of the input queues names. That tells the mapping this model runs for these input data. These topics should entries in Ole.Queue
	OutputQueuesTopics varchar[],          -- Topics of the output queues names. That tells the mapping this model output to give producer queues. These topics should entries in Ole.Queue
	StartDate          Date      not null default current_date, -- Model start date for this customer
	EndDate            Date,                                    -- Model end date for this customer
	IsActive           bool      not null,
	CreateTime         TimeStamp not null default current_timestamp,
	ModifyTime         TimeStamp
    -- , CONSTRAINT Ole_CustomerModel_Model_Fkey  FOREIGN KEY (MdlId) REFERENCES Ole.Model(Mid)
)WITHOUT OIDS;

create table Ole.Configuration
(
	Key        varchar   not null,
	Value      varchar   not null,
	CreateTime TimeStamp not null default current_timestamp,
	ModifyTime TimeStamp
)WITHOUT OIDS;

create table Ole.Queue
(
    Qid             serial    not null,               -- Uniqe id Generates automatically
	Type            varchar   not null,               -- Producer or Consumer
	Topic           varchar   not null,               -- Topic of the queue
	CustomerName    varchar   not null,               -- Curstomer Name to this queue topic. This should be same as Ole.CustomerModel.CustomerName. That is how we relate which models need to execute for this
	ConnectString   varchar   not null,               -- Comma separated HOST:PORT[,HOST:PORT...] -- Brokers List for Producers and  Zookeepers for Consumers
	GroupOrClientId varchar   not null,               -- ClientId For Producers, the client id is a user-specified string sent in each request to help trace calls. For Consumers GroupId, A string that uniquely identifies the group of consumer processes to which this consumer belongs. By setting the same group id multiple processes indicate that they are all part of the same consumer group.
	MaxRetries      int       not null default 3,     -- For Producers, Message Send Max Retries, default 3
	RequiredAcks    int       not null default 0,     -- For Producers, request.required.acks
	IsActive        bool      not null default true,
	CreateTime      TimeStamp not null default current_timestamp,
	ModifyTime      TimeStamp,
    CONSTRAINT  Queue_pkey PRIMARY KEY (Topic)
)WITHOUT OIDS;

CREATE TABLE Ole.Cluster
(
    ClusterId    int4 NOT NULL,
    ClusterName  varchar(255) NOT NULL,
    ClusterNotes varchar(2048) NULL,

    createtime   timestamp default current_timestamp,
    modifiedtime timestamp,

    CONSTRAINT  Cluster_pkey PRIMARY KEY (ClusterName),
    CONSTRAINT  Cluster_ClusterId_idx_unique UNIQUE (ClusterId)
) WITHOUT OIDS;

CREATE TABLE Ole.Node
(
    NodeId       int4 NOT NULL check (nodeId > 0 AND nodeId < 65536),
    NodeName     varchar(255) NOT NULL,
    NodeNotes    varchar(2048) NULL,
    ClusterId    int4 NOT NULL default(1),
    RackId       int4 NOT NULL default(1),
    IpAddress    varchar(64),
    ConnPort     int4 not null, -- Connection port where this server will be running
    IsActive     boolean NOT NULL default(true),

    createtime   timestamp default current_timestamp,
    modifiedtime timestamp,

    CONSTRAINT nodes_pkey PRIMARY KEY (NodeName),
    CONSTRAINT nodeid_idx_unique UNIQUE (NodeId),

    CONSTRAINT Node_Cluster_fkey FOREIGN KEY (ClusterId) REFERENCES Ole.Cluster (ClusterId) ON UPDATE RESTRICT ON DELETE RESTRICT
)
WITHOUT OIDS;

CREATE TABLE Ole.StorageVolume
(
	VolumeId     int4 NOT NULL, -- Unique ID of the volumn
	VolumeRoot   varchar(255) NOT NULL, --
    VolumeNotes  varchar(2048) NULL,
	NodeId       int4 NOT NULL default(0), -- Physical node location
    IsActive     boolean NOT NULL default(true),
	canRead      boolean NOT NULL default(true),
	canWrite     boolean NOT NULL default(true),

	createtime   timestamp default current_timestamp,
	modifiedtime timestamp,

	CONSTRAINT StorageVolumes_pkey PRIMARY KEY (VolumeId),
    CONSTRAINT StorageVolume_Node_fkey FOREIGN KEY (NodeId) REFERENCES Ole.Node (NodeId) ON UPDATE RESTRICT ON DELETE RESTRICT
) WITHOUT OIDS;

CREATE OR REPLACE FUNCTION Ole.Cfg(Key_ varchar, Value_ varchar)
RETURNS int4 AS
$$
BEGIN
	If (Select 1 from Ole.Configuration where lower(Value) = lower(Value_) and lower(Key) = lower(Key_)) Then
		return 0;
	End If;

    Update Ole.Configuration Set Value = value_, ModifyTime = current_timestamp where lower(Key)=lower(Key_);

    IF not found THEN
    	insert into Ole.Configuration(Key, Value) values(Key_, Value_);
    End If;

    return 1;
END;
$$ LANGUAGE plpgsql VOLATILE;

CREATE OR REPLACE FUNCTION Ole.RegisterModel(ModelName_ varchar, ModelVersion_ varchar, ModelPmml_ text, ModelJarPath_ varchar, OutputPolicy_ int, IsActive_ bool)
RETURNS int8 AS
$$
DECLARE
  MdlId_ int8;
BEGIN
	-- Check whether any of the fields are invalid ones like empty or something like that
	SELECT INTO MdlId_ Mid FROM Ole.Model WHERE lower(Name) = lower(ModelName_);

	IF (NOT FOUND) THEN
		INSERT INTO Ole.Model (Name, IsActive) VALUES (lower(ModelName_), IsActive_);
		SELECT INTO MdlId_ Mid FROM Ole.Model WHERE lower(Name) = lower(ModelName_);
		INSERT INTO Ole.ModelVersion (MdlId, MdlPmml, MdlVersion, JarPath, OutputPolicy, IsActive) VALUES (MdlId_, ModelPmml_, ModelVersion_, ModelJarPath_, OutputPolicy_, IsActive_);
	ELSE
	    Update Ole.ModelVersion Set MdlPmml = ModelPmml_, JarPath = ModelJarPath_, OutputPolicy = OutputPolicy_, IsActive = IsActive_, ModifyTime = current_timestamp where MdlId = MdlId_ and lower(MdlVersion) = lower(ModelVersion_);
		IF not found THEN
			INSERT INTO Ole.ModelVersion(MdlId, MdlPmml, MdlVersion, JarPath, OutputPolicy, IsActive) VALUES (MdlId_, ModelPmml_, ModelVersion_, ModelJarPath_, OutputPolicy_, IsActive_);
		End If;
	END IF;

    return MdlId_;
END;
$$ LANGUAGE plpgsql VOLATILE;

-- Only one version per customer at this moment
CREATE OR REPLACE FUNCTION Ole.RegisterModelToCustomer(ModelName_ varchar, ModelVersion_ varchar, CustomerName_ varchar, StartDate_ Date, EndDate_ Date, IsActive_ bool, InputQueuesTopics_ varchar[], OutputQueuesTopics_ varchar[])
RETURNS int4 AS
$$
DECLARE
  MdlId_ int8;
  VerNo varchar;
BEGIN
	-- Check whether any of the fields are invalid ones like empty or something like that
	SELECT INTO MdlId_ Mid FROM Ole.Model WHERE lower(Name) = lower(ModelName_);
	IF (NOT FOUND) THEN
		RAISE EXCEPTION 'Model % not found.', ModelName_;
	END IF;

	SELECT INTO VerNo MdlVersion FROM Ole.ModelVersion WHERE MdlId = MdlId_ and lower(MdlVersion) = lower(ModelVersion_);
	IF (NOT FOUND) THEN
		RAISE EXCEPTION 'Model % and Version % not found.', ModelName_, ModelVersion_;
	END IF;

	Update Ole.CustomerModel Set StartDate = StartDate_, EndDate = EndDate_, IsActive = IsActive_, InputQueuesTopics = InputQueuesTopics_, OutputQueuesTopics = OutputQueuesTopics_, ModifyTime = current_timestamp where MdlId = MdlId_ and lower(MdlVersion) = lower(ModelVersion_) and lower(CustomerName) = lower(CustomerName_);
	IF not found THEN
		INSERT INTO Ole.CustomerModel(MdlId, MdlVersion, CustomerName, StartDate, EndDate, IsActive, InputQueuesTopics, OutputQueuesTopics) VALUES (MdlId_, ModelVersion_, CustomerName_, StartDate_, EndDate_, IsActive_, InputQueuesTopics_, OutputQueuesTopics_);
	End If;

    return MdlId_;
END;
$$ LANGUAGE plpgsql VOLATILE;

CREATE OR REPLACE FUNCTION Ole.UpdateActiveFlagForModel(ModelName_ varchar, IsActive_ bool)
RETURNS int4 AS
$$
BEGIN
	-- Check whether any of the fields are invalid ones like empty or something like that

	Update Ole.Model Set IsActive = IsActive_, ModifyTime = current_timestamp where lower(Name) = lower(ModelName_);

	IF (NOT FOUND) THEN
		RAISE EXCEPTION 'Model % not found.', ModelName_;
	END IF;

    return 0;
END;
$$ LANGUAGE plpgsql VOLATILE;

CREATE OR REPLACE FUNCTION Ole.UpdateActiveFlagForModelVersion(ModelName_ varchar, ModelVersion_ varchar, IsActive_ bool)
RETURNS int4 AS
$$
DECLARE
  MdlId_ int8;
BEGIN
	-- Check whether any of the fields are invalid ones like empty or something like that
	SELECT INTO MdlId_ Mid FROM Ole.Model WHERE lower(Name) = lower(ModelName_);
	IF (NOT FOUND) THEN
		RAISE EXCEPTION 'Model % not found.', ModelName_;
	END IF;

	Update Ole.ModelVersion Set IsActive = IsActive_, ModifyTime = current_timestamp where MdlId = MdlId_ and lower(MdlVersion) = lower(ModelVersion_);

	IF (NOT FOUND) THEN
		RAISE EXCEPTION 'Model % and Version % not found.', ModelName_, ModelVersion_;
	END IF;

    return 0;
END;
$$ LANGUAGE plpgsql VOLATILE;

CREATE OR REPLACE FUNCTION Ole.UpdateActiveFlagForCustomer(ModelName_ varchar, ModelVersion_ varchar, CustomerName_ varchar, IsActive_ bool)
RETURNS int4 AS
$$
DECLARE
  MdlId_ int8;
BEGIN
	-- Check whether any of the fields are invalid ones like empty or something like that
	SELECT INTO MdlId_ Mid FROM Ole.Model WHERE lower(Name) = lower(ModelName_);
	IF (NOT FOUND) THEN
		RAISE EXCEPTION 'Model % not found.', ModelName_;
	END IF;

	Update Ole.CustomerModel Set IsActive = IsActive_, ModifyTime = current_timestamp where MdlId = MdlId_ and lower(MdlVersion) = lower(ModelVersion_) and lower(CustomerName) = lower(CustomerName_);

	IF (NOT FOUND) THEN
		RAISE EXCEPTION 'Model %, Version % and customer % not found.', ModelName_, ModelVersion_, CustomerName_;
	END IF;

    return 0;
END;
$$ LANGUAGE plpgsql VOLATILE;

-- INSERT INTO Ole.Cluster(ClusterId, ClusterName) values (1, 'Ole Cluster 1');

-- INSERT INTO Ole.Node(NodeId, NodeName, ClusterId, RackId, IpAddress, ConnPort, IsActive) values (1, 'mira01', 1, 1, '10.100.0.21', 6543, true);

-- INSERT INTO Ole.StorageVolume(VolumeId, VolumeRoot, NodeId, IsActive, canRead, canWrite) values (10101, '/mnt/ssd1/mira/ole/demodata', 1, true, true, true);
-- INSERT INTO Ole.StorageVolume(VolumeId, VolumeRoot, NodeId, IsActive, canRead, canWrite) values (10201, '/mnt/ssd2/mira/ole/demodata', 1, true, true, true);
-- INSERT INTO Ole.StorageVolume(VolumeId, VolumeRoot, NodeId, IsActive, canRead, canWrite) values (10301, '/mnt/ssd3/mira/ole/demodata', 1, true, true, true);
-- INSERT INTO Ole.StorageVolume(VolumeId, VolumeRoot, NodeId, IsActive, canRead, canWrite) values (10401, '/mnt/ssd4/mira/ole/demodata', 1, true, true, true);

-- INSERT INTO Ole.Node(NodeId, NodeName, ClusterId, RackId, IpAddress, ConnPort, IsActive) values (2, 'mira01', 1, 1, '127.0.0.1', 6543, true);

-- INSERT INTO Ole.StorageVolume(VolumeId, VolumeRoot, NodeId, IsActive, canRead, canWrite) values (20101, '/home/rich/ole/demo/ostore1', 2, true, true, true);
-- INSERT INTO Ole.StorageVolume(VolumeId, VolumeRoot, NodeId, IsActive, canRead, canWrite) values (20201, '/home/rich/ole/demo/ostore2', 2, true, true, true);
-- INSERT INTO Ole.StorageVolume(VolumeId, VolumeRoot, NodeId, IsActive, canRead, canWrite) values (20301, '/home/rich/ole/demo/ostore3', 2, true, true, true);

-- Getting volumes for one node
-- select VolumeRoot from Ole.StorageVolume where IsActive and NodeId in (select nodeid from Ole.Node where IsActive and NodeName ilike 'devdb01');

-- INSERT INTO Ole.Queue(Type, Topic, CustomerName, ConnectString, GroupOrClientId, MaxRetries, RequiredAcks, IsActive) VALUES ('producer', 'clinicaloutput', 'BSC', '127.0.0.1:9093', 'ClientBsc', 3, 0, true);

-- INSERT INTO Ole.Queue(Type, Topic, CustomerName, ConnectString, GroupOrClientId, MaxRetries, RequiredAcks, IsActive) VALUES ('consumer', 'clinicalinput', 'BSC', '127.0.0.1:2181', 'clinical', 0, 0, true);

-- INSERT INTO Ole.Queue(Type, Topic, CustomerName, ConnectString, GroupOrClientId, MaxRetries, RequiredAcks, IsActive) VALUES ('consumer', 'clinicalbsc', 'BSC', '127.0.0.1:2181', 'clinicalbsc', 0, 0, true);

-- select Ole.RegisterModel('AsthmaRiskMarker', '0.0.1', 'PMML String Here', '/mnt/ssd1/mira/ole/models/ModelAsthma.jar', 1, true);
-- select Ole.RegisterModelToCustomer('AsthmaRiskMarker', '0.0.1', 'BSC', '2010-01-01', '2015-01-01', true, '{clinicalinput}', '{clinicaloutput}');

-- select Ole.RegisterModel('CadRiskMarker', '0.0.1', 'PMML String Here', '/mnt/ssd1/mira/ole/models/ModelCad.jar', 2, true);
-- select Ole.RegisterModelToCustomer('CadRiskMarker', '0.0.1', 'BSC', '2010-01-01', '2015-01-01', true, '{clinicalinput}', '{clinicaloutput}');

-- select Ole.RegisterModel('BscContraIndicative', '0.0.1', 'PMML String Here', '/mnt/ssd1/mira/ole/models/ModelBscContraIndicative.jar', 3, true);
-- select Ole.RegisterModelToCustomer('BscContraIndicative', '0.0.1', 'BSC', '2010-01-01', '2015-01-01', true, '{clinicalinput}', '{clinicaloutput}');

-- select m.Name as modelname, v.MdlVersion, v.JarPath, v.OutputPolicy, c.CustomerName, 
-- (extract(year from c.StartDate) * 10000 + extract(month from c.StartDate) * 100 + extract(day from c.StartDate)) as StartDate,
-- (extract(year from c.EndDate) * 10000 + extract(month from c.EndDate) * 100 + extract(day from c.EndDate)) as EndDate,
-- array_to_string(c.InputQueuesTopics, ',') as InputQueuesTopics, array_to_string(c.OutputQueuesTopics, ',') as OutputQueuesTopics
-- from Ole.Model as m inner join Ole.ModelVersion as v on (m.Mid = v.MdlId) inner join Ole.CustomerModel as c on (v.MdlId = c.MdlId and v.MdlVersion = c.MdlVersion) 
-- where m.IsActive and v.IsActive and c.IsActive;


INSERT INTO Ole.Cluster(ClusterId, ClusterName) values (1, 'MiraCluster');
INSERT INTO Ole.Node(NodeId, NodeName, ClusterId, RackId, IpAddress, ConnPort, IsActive) values (2, 'mira01', 1, 1, '127.0.0.1', 6543, true);
INSERT INTO Ole.StorageVolume(VolumeId, VolumeRoot, NodeId, IsActive, canRead, canWrite) values (20101, '/home/rich/ole/demo/ostore1', 2, true, true, true);
INSERT INTO Ole.StorageVolume(VolumeId, VolumeRoot, NodeId, IsActive, canRead, canWrite) values (20201, '/home/rich/ole/demo/ostore2', 2, true, true, true);
INSERT INTO Ole.StorageVolume(VolumeId, VolumeRoot, NodeId, IsActive, canRead, canWrite) values (20301, '/home/rich/ole/demo/ostore3', 2, true, true, true);



