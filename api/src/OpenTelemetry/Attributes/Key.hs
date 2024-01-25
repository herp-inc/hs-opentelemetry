{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

{- |
Module      :  OpenTelemetry.Attributes.Key
Copyright   :  (c) Kazuki Okamoto (岡本和樹), 2023
License     :  BSD-3
Description :  Key-value pair metadata used in 'OpenTelemetry.Trace.Span's, 'OpenTelemetry.Trace.Link's, and 'OpenTelemetry.Trace.Event's
Maintainer  :  Kazuki Okamoto (岡本和樹)
Stability   :  experimental
Portability :  non-portable (GHC extensions)
-}
module OpenTelemetry.Attributes.Key (
  Key (..),
  forget,

  -- * Semantic conventions
  -- $semanticConversions

  -- ** Attributes Registry
  -- $attributesRegistry

  -- *** Cloud
  -- $tbd

  -- *** Code
  -- $code
  code_column,
  code_filepath,
  code_function,
  code_lineno,
  code_namespace,

  -- *** Container
  -- $container
  container_command,
  container_commandArgs,
  container_commandLine,
  container_id,
  container_image_id,
  container_image_name,
  container_image_repoDigests,
  container_image_tags,
  container_name,
  container_runtime,

  -- *** HTTP
  -- $http
  http_request_body_size,
  http_request_method,
  http_request_methodOriginal,
  http_request_resendCount,
  http_response_body_size,
  http_response_statusCode,
  http_route,

  -- *** Messaging
  -- $tbd

  -- *** Network
  -- $network
  network_carrier_icc,
  network_carrier_mcc,
  network_carrier_mnc,
  network_carrier_name,
  network_connection_subtype,
  network_connection_type,
  network_local_address,
  network_local_port,
  network_peer_address,
  network_peer_port,
  network_protocol_name,
  network_protocol_version,
  network_transport,
  network_type,

  -- *** Open Container Initiative
  -- $tbd

  -- *** RPC
  -- $rpc
  rpc_connectRpc_errorCode,
  rpc_connectRpc_request_metadata,
  rpc_connectRpc_response_metadata,
  rpc_grpc_request_metadata,
  rpc_grpc_response_metadata,
  rpc_grpc_statusCode,
  rpc_jsonrpc_errorCode,
  rpc_jsonrpc_errorMessage,
  rpc_jsonrpc_requestId,
  rpc_jsonrpc_version,
  rpc_method,
  rpc_service,
  rpc_system,

  -- *** Thread
  -- $tbd

  -- *** URL
  -- $tbd

  -- *** User agent
  -- $tbd

  -- ** General Attributes
  -- $generalAttributes

  -- *** Server, client and shared network attributes
  -- $serverClientSharedNetworkAttributes
  server_address,
  server_port,
  client_address,
  client_port,

  -- *** Source and destination attributes
  -- $sourceAndDestinationAttributes
  source_address,
  source_port,
  destination_address,
  destination_port,

  -- *** General remote service attributes
  -- $generalRemoteServiceAttributes
  peer_service,

  -- *** General identity attributes
  -- $generalIdentityAttributes
  enduser_id,
  enduser_role,
  enduser_scope,

  -- ** Event Attributes
  -- $eventAttributes

  -- *** General event attributes
  -- $generalEventAttributes
  event_domain,
  event_name,

  -- ** General Logs Attributes
  -- $generalLogsAttributes

  -- *** General log identification attributes
  -- $generalLogIdentificationAttributes
  log_record_uid,

  -- *** Log media
  -- $logMedia
  log_file_name,
  log_file_nameResolved,
  log_file_path,
  log_file_pathResolved,
  log_iostream,

  -- ** Session

  -- *** Attributes
  session_id,
  session_previousId,

  -- ** Tracing Compatibility Components

  -- *** OpenTracing
  opentracing_refType,

  -- ** Cloud Providers

  -- *** AWS SDK
  -- $tbd

  -- ** CloudEvents

  -- *** CloudEvents Spans
  -- $tbd

  -- ** Database Calls and Systems

  -- *** AWS DynamoDB
  -- $tbd

  -- *** Cassandra
  -- $tbd

  -- *** Database Client Calls
  -- $databaseClientCalls
  db_connectionString,
  db_system,
  db_user,
  db_name,
  db_operation,
  db_statement,

  -- *** Microsoft Cosmos DB
  -- $tbd

  -- *** CouchDB
  -- $tbd

  -- *** Elasticsearch
  -- $tbd

  -- *** GraphQL Server
  -- $tbd

  -- *** HBase
  -- $tbd

  -- *** Database Metrics
  -- $tbd

  -- *** MongoDB
  -- $tbd

  -- *** MSSQL
  -- $tbd

  -- *** Redis
  -- $redis
  db_redis_databaseIndex,

  -- *** SQL Database
  -- $sqlDatabase
  db_sql_table,

  -- ** Exceptions

  -- *** Exceptions in logs
  -- $tbd

  -- *** Exceptions in spans
  -- $tbd

  -- ** Function-as-a-Service
  -- $tbd

  -- ** Feature Flags
  -- $tbd

  -- ** HTTP

  -- *** HTTP metrics
  -- $httpMetrics
  error_type,

  -- *** HTTP spans
  -- $httpSpans

  -- ** Messaging Systems
  -- $tbd

  -- ** Mobile Platform
  -- $tbd

  -- ** Object Stores
  -- $tbd

  -- ** Resource
  -- $tbd

  -- *** Function as a Service
  -- $faas
  faas_instance,
  faas_maxMemory,
  faas_name,
  faas_version,

  -- ** RPC
  -- $tbd

  -- ** Runtime Environment
  -- $tbd

  -- ** System
  -- $tbd

  -- ** URL
  -- $tbd
) where

import Data.Int (Int64)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import OpenTelemetry.Attributes.Attribute (Attribute)


{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


newtype Key a = Key {unkey :: Text} deriving stock (Show, Eq, Ord, Generic)


-- | Raise an error if the string is empty.
instance IsString (Key a) where
  fromString "" = error "Key cannot be empty"
  fromString s = Key $ T.pack s


forget :: Key a -> Key Attribute
forget = Key . unkey


{- $semanticConversions

An Attribute is a key-value pair, which MUST have the following properties:

    The attribute key MUST be a non-@null@ and non-empty string.

- The attribute value is either:

    - A primitive type: string, boolean, double precision floating point (IEEE 754-1985) or signed 64 bit integer.
    - An array of primitive type values. The array MUST be homogeneous, i.e., it MUST NOT contain values of different types. For protocols that do not natively support array values such values SHOULD be represented as JSON strings.

Attribute values expressing a numerical value of zero, an empty string, or an empty array are considered meaningful and MUST be stored and passed on to processors \/ exporters.

Specification: https://opentelemetry.io/docs/specs/otel/common/
-}


{- $attributesRegistry

The attributes registry is the place where attributes are defined.

Specification: https://opentelemetry.io/docs/specs/semconv/attributes-registry/
-}


{- $code
These attributes allow to report this unit of code and therefore to provide more context about the telemetry data.

Specification: https://opentelemetry.io/docs/specs/semconv/attributes-registry/code/
-}


{- | The column number in @code.filepath@ best representing the operation.
It SHOULD point within the code unit named in @code.function@.
-}
code_column :: Key Int64
code_column = "code.column"


-- | The source code file name that identifies the code unit as uniquely as possible (preferably an absolute file path).
code_filepath :: Key Text
code_filepath = "code.filepath"


-- | The method or function name, or equivalent (usually rightmost part of the code unit's name).
code_function :: Key Text
code_function = "code.function"


{- | The line number in @code.filepath@ best representing the operation.
It SHOULD point within the code unit named in @code.function@.
-}
code_lineno :: Key Int64
code_lineno = "code.lineno"


{- | The “namespace” within which @code.function@ is defined.
Usually the qualified class or module name, such that @code.namespace@ + some separator + @code.function@ form a unique identifier for the code unit.
-}
code_namespace :: Key Text
code_namespace = "code.namespace"


{- $container
Specification: https://opentelemetry.io/docs/specs/semconv/attributes-registry/container/
-}


-- | The command used to run the container (i.e. the command name).
container_command :: Key Text
container_command = "container.command"


-- | All the command arguments (including the command\/executable itself) run by the container.
container_commandArgs :: Key [Text]
container_commandArgs = "container.command_args"


-- | The full command run by the container as a single string representing the full command.
container_commandLine :: Key Text
container_commandLine = "container.command_line"


{- | Container ID. Usually a UUID, as for example used to (identify Docker containers)[https://docs.docker.com/engine/reference/run/#container-identification].
The UUID might be abbreviated.
-}
container_id :: Key Text
container_id = "container.id"


-- | Runtime specific image identifier. Usually a hash algorithm followed by a UUID.
container_image_id :: Key Text
container_image_id = "container.image.id"


-- | Name of the image the container was built on.
container_image_name :: Key Text
container_image_name = "container.image.name"


-- | Repo digests of the container image as provided by the container runtime.
container_image_repoDigests :: Key [Text]
container_image_repoDigests = "container.image.repo_digests"


{- | Container image tags. An example can be found in [Docker Image Inspect](https://docs.docker.com/engine/api/v1.43/#tag/Image/operation/ImageInspect).
Should be only the @<tag>@ section of the full name for example from @registry.example.com/my-org/my-image:<tag>@.
-}
container_image_tags :: Key [Text]
container_image_tags = "container.image.tags"


-- | Container name used by container runtime.
container_name :: Key Text
container_name = "container.name"


-- | The container runtime managing this container.
container_runtime :: Key Text
container_runtime = "container.runtime"


{- $http
Specification: https://opentelemetry.io/docs/specs/semconv/attributes-registry/http/
-}


-- | The size of the request payload body in bytes.
http_request_body_size :: Key Int64
http_request_body_size = "http.request.body.size"


-- | HTTP request method.
http_request_method :: Key Text
http_request_method = "http.request.method"


-- | Original HTTP method sent by the client in the request line.
http_request_methodOriginal :: Key Text
http_request_methodOriginal = "http.request.method_original"


-- | The ordinal number of request resending attempt (for any reason, including redirects).
http_request_resendCount :: Key Int64
http_request_resendCount = "http.request.resend_count"


-- | The size of the response payload body in bytes.
http_response_body_size :: Key Int64
http_response_body_size = "http.response.body.size"


-- | [HTTP response status code](https://tools.ietf.org/html/rfc7231#section-6).
http_response_statusCode :: Key Int64
http_response_statusCode = "http.response.status_code"


-- | The matched route, that is, the path template in the format used by the respective server framework.
http_route :: Key Text
http_route = "http.route"


-- | The ISO 3166-1 alpha-2 2-character country code associated with the mobile carrier network.
network_carrier_icc :: Key Text
network_carrier_icc = "network.carrier.icc"


-- | The mobile carrier country code.
network_carrier_mcc :: Key Text
network_carrier_mcc = "network.carrier.mcc"


-- | The mobile carrier network code.
network_carrier_mnc :: Key Text
network_carrier_mnc = "network.carrier.mnc"


-- | The name of the mobile carrier.
network_carrier_name :: Key Text
network_carrier_name = "network.carrier.name"


-- | This describes more details regarding the @network.connection.type@.
network_connection_subtype :: Key Text
network_connection_subtype = "network.connection.subtype"


-- | The internet connection type.
network_connection_type :: Key Text
network_connection_type = "network.connection.type"


-- | Local address of the network connection - IP address or Unix domain socket name.
network_local_address :: Key Text
network_local_address = "network.local.address"


-- | Local port number of the network connection.
network_local_port :: Key Int64
network_local_port = "network.local.port"


-- | Peer address of the network connection - IP address or Unix domain socket name.
network_peer_address :: Key Text
network_peer_address = "network.peer.address"


-- | Peer port number of the network connection.
network_peer_port :: Key Int64
network_peer_port = "network.peer.port"


-- | OSI application layer or non-OSI equivalent.
network_protocol_name :: Key Text
network_protocol_name = "network.protocol.name"


-- | Version of the protocol specified in @network.protocol.name@.
network_protocol_version :: Key Text
network_protocol_version = "network.protocol.version"


-- | OSI transport layer or inter-process communication method.
network_transport :: Key Text
network_transport = "network.transport"


-- | OSI network layer or non-OSI equivalent.
network_type :: Key Text
network_type = "network.type"

{- $rpc
RPC attributes are intended to be used in the context of events related to remote procedure calls (RPC).

Specification: https://opentelemetry.io/docs/specs/semconv/attributes-registry/rpc/
-}

-- | The [error codes](https://connect.build/docs/protocol/#error-codes) of the Connect request. Error codes are always string values.
rpc_connectRpc_errorCode :: Key Text
rpc_connectRpc_errorCode = "rpc.connect_rpc.error_code"

-- | Connect request metadata, @key@ being the normalized Connect Metadata key (lowercase), the value being the metadata values.
rpc_connectRpc_request_metadata :: Text -> Key [Text]
rpc_connectRpc_request_metadata key = Key $ "rpc.connect_rpc.request_metadata." <> key

-- | Connect response metadata, @key@ being the normalized Connect Metadata key (lowercase), the value being the metadata values.
rpc_connectRpc_response_metadata :: Text -> Key [Text]
rpc_connectRpc_response_metadata key = Key $ "rpc.connect_rpc.response_metadata." <> key

-- | gRPC request metadata, @key@ being the normalized gRPC Metadata key (lowercase), the value being the metadata values.
rpc_grpc_request_metadata :: Text -> Key [Text]
rpc_grpc_request_metadata key = Key $ "rpc.grpc.request_metadata." <> key

-- | gRPC response metadata, @key@ being the normalized gRPC Metadata key (lowercase), the value being the metadata values.
rpc_grpc_response_metadata :: Text -> Key [Text]
rpc_grpc_response_metadata key = Key $ "rpc.grpc.response_metadata." <> key

-- | The [numeric status code](https://github.com/grpc/grpc/blob/v1.33.2/doc/statuscodes.md) of the gRPC request.
rpc_grpc_statusCode :: Key Int64
rpc_grpc_statusCode = "rpc.grpc.status_code"

-- | @error.code@ property of response if it is an error response.
rpc_jsonrpc_errorCode :: Key Int64
rpc_jsonrpc_errorCode = "rpc.jsonrpc.error_code"

-- | @error.message@ property of response if it is an error response.
rpc_jsonrpc_errorMessage :: Key Text
rpc_jsonrpc_errorMessage = "rpc.jsonrpc.error_message"


-- | @id@ property of request or response. Since protocol allows id to be int, string, null or missing (for notifications), value is expected to be cast to string for simplicity. Use empty string in case of null value. Omit entirely if this is a notification.
rpc_jsonrpc_requestId :: Key Text
rpc_jsonrpc_requestId = "rpc.jsonrpc.request_id"


-- | Protocol version as in @jsonrpc@ property of request/response. Sinse JSON-RPC 1.0 does not specify this, the value can be omitted.
rpc_jsonrpc_version :: Key Text
rpc_jsonrpc_version = "rpc.jsonrpc.version"


-- | The name of the (logical) method being called, must be equal to the $method part in the span name.
rpc_method :: Key Text
rpc_method = "rpc.method"

-- | The full (logical) name of the service being called, including its package name, if applicable.
rpc_service :: Key Text
rpc_service = "rpc.service"

-- | A string identifying the remoting system. See below for a list of well-known identifiers.
rpc_system :: Key Text
rpc_system = "rpc.system"


{- $generalAttributes
The attributes described in this section are not specific to a particular operation but rather generic. They may be used in any Span they apply to. Particular operations may refer to or require some of these attributes.

Specification: https://opentelemetry.io/docs/specs/semconv/general/attributes/
-}


{- $serverClientSharedNetworkAttributes
These attributes may be used to describe the client and server in a connection-based network interaction where there is one side that initiates the connection (the client is the side that initiates the connection). This covers all TCP network interactions since TCP is connection-based and one side initiates the connection (an exception is made for peer-to-peer communication over TCP where the “user-facing” surface of the protocol \/ API does not expose a clear notion of client and server). This also covers UDP network interactions where one side initiates the interaction, e.g. QUIC (HTTP\/3) and DNS.
-}


{- | Server domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name.

Recomended.
-}
server_address :: Key Text
server_address = "server.address"


{- | Server port number.

Recomended.
-}
server_port :: Key Int64
server_port = "server.port"


{- | Client address - domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name.

Recomended.
-}
client_address :: Key Text
client_address = "client.address"


{- | Client port number.

Recomended.
-}
client_port :: Key Int64
client_port = "client.port"


{- $sourceAndDestinationAttributes
These attributes may be used to describe the sender and receiver of a network exchange\/packet. These should be used when there is no client\/server relationship between the two sides, or when that relationship is unknown. This covers low-level network interactions (e.g. packet tracing) where you don’t know if there was a connection or which side initiated it. This also covers unidirectional UDP flows and peer-to-peer communication where the “user-facing” surface of the protocol \/ API does not expose a clear notion of client and server.
-}


{- | Source address - domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name.

Recomended.
-}
source_address :: Key Text
source_address = "source.address"


{- | Source port number.

Recomended.
-}
source_port :: Key Int64
source_port = "source.port"


{- | Destination address - domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name.

Recomended.
-}
destination_address :: Key Text
destination_address = "destination.address"


{- | Destination port number.

Recomended.
-}
destination_port :: Key Int64
destination_port = "destination.port"


{- $generalRemoteServiceAttributes
This attribute may be used for any operation that accesses some remote service. Users can define what the name of a service is based on their particular semantics in their distributed system. Instrumentations SHOULD provide a way for users to configure this name.
-}


{- | The @service.name@ of the remote service. SHOULD be equal to the actual @service.name@ resource attribute of the remote service if any.

Recomended.
-}
peer_service :: Key Text
peer_service = "peer.service"


{- | Username or client_id extracted from the access token or [Authorization](https://tools.ietf.org/html/rfc7235#section-4.2) header in the inbound request from outside the system.

Recomended.
-}
enduser_id :: Key Text
enduser_id = "enduser.id"


{- | Actual\/assumed role the client is making the request under extracted from token or application security context.

Recomended.
-}
enduser_role :: Key Text
enduser_role = "enduser.role"


{- | Scopes or granted authorities the client currently possesses extracted from token or application security context. The value would come from the scope associated with an [OAuth 2.0 Access](https://tools.ietf.org/html/rfc6749#section-3.3) Token or an attribute value in a [SAML 2.0 Assertion](http://docs.oasis-open.org/security/saml/Post2.0/sstc-saml-tech-overview-2.0.html).

Recomended.
-}
enduser_scope :: Key Text
enduser_scope = "enduser.scope"


{- $eventAttributes
Specification: https://opentelemetry.io/docs/specs/semconv/general/events/
-}


{- $generalLogIdentificationAttributes
Events are recorded as LogRecords that are shaped in a special way: Event LogRecords have the attributes event.domain and event.name (and possibly other LogRecord attributes).
-}


{- | The domain identifies the business context for the events.

Required.
-}
event_domain :: Key Text
event_domain = "event.domain"


{- | The name identifies the event.

Required.
-}
event_name :: Key Text
event_name = "event.name"


{- $generalLogsAttributes
The attributes described in this section are rather generic. They may be used in any Log Record they apply to.

Specification: https://opentelemetry.io/docs/specs/semconv/general/logs/
-}


{- | A unique identifier for the Log Record.

Opt-In.
-}
log_record_uid :: Key Text
log_record_uid = "log.record.uid"


{- $logMedia
This section describes attributes for log media in OpenTelemetry. Log media are mechanisms by which logs are transmitted. Types of media include files, streams, network protocols, and os-specific logging services such as journald and Windows Event Log.
-}


{- | The basename of the file.

Recommended.
-}
log_file_name :: Key Text
log_file_name = "log.file.name"


{- | The basename of the file, with symlinks resolved.

Opt-In.
-}
log_file_nameResolved :: Key Text
log_file_nameResolved = "log.file.name_resolved"


{- | The full path to the file.

Opt-In.
-}
log_file_path :: Key Text
log_file_path = "log.file.path"


{- | The full path to the file, with symlinks resolved.

Opt-In.
-}
log_file_pathResolved :: Key Text
log_file_pathResolved = "log.file.path_resolved"


{- | The stream associated with the log.

Opt-In.
-}
log_iostream :: Key Text
log_iostream = "log.iostream"


{- | A unique id to identify a session.

Opt-In.
-}
session_id :: Key Text
session_id = "session.id"


{- | The previous @session.id@ for this user, when known.

Opt-In.
-}
session_previousId :: Key Text
session_previousId = "session.previous_id"


{- | Parent-child reference type.

Recommended.
-}
opentracing_refType :: Key Text
opentracing_refType = "opentracing.ref_type"


{- $databaseClientCalls
Span kind: MUST always be CLIENT.

The span name SHOULD be set to a low cardinality value representing the statement executed on the database. It MAY be a stored procedure name (without arguments), DB statement without variable arguments, operation name, etc. Since SQL statements may have very high cardinality even without arguments, SQL spans SHOULD be named the following way, unless the statement is known to be of low cardinality: @db.operation@ @db.name.db.sql.table@, provided that @db.operation@ and @db.sql.table@ are available. If @db.sql.table@ is not available due to its semantics, the span SHOULD be named @db.operation@ @db.name@. It is not recommended to attempt any client-side parsing of @db.statement@ just to get these properties, they should only be used if the library being instrumented already provides them. When it’s otherwise impossible to get any meaningful span name, @db.name@ or the tech-specific database name MAY be used.

Specification: https://opentelemetry.io/docs/specs/semconv/database/database-spans/
-}


{- | The connection string used to connect to the database. It is recommended to remove embedded credentials.

Recommended.
-}
db_connectionString :: Key Text
db_connectionString = "db.connection_string"


{- | An identifier for the database management system (DBMS) product being used. See the spec. for a list of well-known identifiers.

Required.
-}
db_system :: Key Text
db_system = "db.system"


{- | Username for accessing the database.

Recomended.
-}
db_user :: Key Text
db_user = "db.user"


{-
db.name	string	This attribute is used to report the name of the database being accessed. For commands that switch the database, this should be set to the target database (even if the command fails). [1]	customers; main	Conditionally Required: If applicable.
db.operation	string	The name of the operation being executed, e.g. the MongoDB command name such as findAndModify, or the SQL keyword. [2]	findAndModify; HMSET; SELECT	Conditionally Required: If db.statement is not applicable.
db.statement	string	The database statement being executed.	SELECT * FROM wuser_table; SET mykey "WuValue"	Recommended: [3]
-}

{- | This attribute is used to report the name of the database being accessed. For commands that switch the database, this should be set to the target database (even if the command fails).

Conditionally Required: If applicable.
-}
db_name :: Key Text
db_name = "db.name"


{- | The name of the operation being executed, e.g. the [MongoDB command name](https://docs.mongodb.com/manual/reference/command/#database-operations) such as @findAndModify@, or the SQL keyword.

Conditionally Required: If @db.statement@ is not applicable.
-}
db_operation :: Key Text
db_operation = "db.operation"


{- | The database statement being executed.

Recommended.
-}
db_statement :: Key Text
db_statement = "db.statement"


{- $redis
The Semantic Conventions for [Redis](https://redis.com/) extend and override the [Database Semantic Conventions](https://opentelemetry.io/docs/specs/semconv/database/database-spans/) that describe common database operations attributes in addition to the Semantic Conventions described on this page.

@db.system@ MUST be set to @"redis"@.
-}


{- | The index of the database being accessed as used in the [@SELECT@ command](https://redis.io/commands/select), provided as an integer. To be used instead of the generic @db.name@ attribute.}

Conditionally Required: If other than the default database (@0@).
-}
db_redis_databaseIndex :: Key Int64
db_redis_databaseIndex = "db.redis.database_index"


{- $sqlDatabase
The SQL databases Semantic Conventions extend and override the [Database Semantic Conventions](https://opentelemetry.io/docs/specs/semconv/database/database-spans/) that describe common database operations attributes in addition to the Semantic Conventions described on this page.
-}


{- | The name of the primary table that the operation is acting upon, including the database name (if applicable).

Recommended.
-}
db_sql_table :: Key Text
db_sql_table = "db.sql.table"


{- $httpMetrics
The conventions described in this section are HTTP specific. When HTTP operations occur, metric events about those operations will be generated and reported to provide insight into the operations. By adding HTTP attributes to metric events it allows for finely tuned filtering.

Specification: https://opentelemetry.io/docs/specs/semconv/http/http-metrics/
-}


{- | Describes a class of error the operation ended with.

Conditionally Required: If request has ended with an error.
-}
error_type :: Key Text
error_type = "error.type"


{- $faas
Specification: https://opentelemetry.io/docs/specs/semconv/resource/faas/
-}


{-
faas.instance	string	The execution environment ID as a string, that will be potentially reused for other invocations to the same function/function version. [2]	2021/06/28/[$LATEST]2f399eb14537447da05ab2a2e39309de	Recommended
faas.max_memory	int	The amount of memory available to the serverless function converted to Bytes. [3]	134217728	Recommended
faas.name	string	The name of the single function that this runtime instance executes. [4]	my-function; myazurefunctionapp/some-function-name	Required
faas.version	string	The immutable version of the function being executed. [5]	26; pinkfroid-00002	Recommended
-}

{- | The execution environment ID as a string, that will be potentially reused for other invocations to the same function\/function version.

Recomended.
-}
faas_instance :: Key Text
faas_instance = "faas.instance"


{- | The amount of memory available to the serverless function converted to Bytes.

Recomended.
-}
faas_maxMemory :: Key Int64
faas_maxMemory = "faas.max_memory"


{- | The name of the single function that this runtime instance executes.

Required.
-}
faas_name :: Key Text
faas_name = "faas.name"


{- | The immutable version of the function being executed.

Recomended.
-}
faas_version :: Key Text
faas_version = "faas.version"


{- $tbd

To be done.
-}
