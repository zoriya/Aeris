import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'package:aeris/src/models/action.dart';
import 'package:aeris/src/models/action_template.dart';
import 'package:aeris/src/models/pipeline.dart';
import 'package:aeris/src/models/reaction.dart';
import 'package:aeris/src/models/service.dart';
import 'package:aeris/src/models/trigger.dart';
import 'package:path_provider/path_provider.dart';
import 'package:http/http.dart' as http;

extension IsOk on http.Response {
  bool get ok => (statusCode ~/ 100) == 2;
}

/// Requests types supported by Aeris API
enum AerisAPIRequestType { get, post, put, delete }

/// Call to interact with Aeris' Back end
class AerisAPI {
  /// Get Connection state
  bool _connected = false;
  bool get isConnected => _connected;

  late List<Pipeline> fakeAPI;

  /// JWT token used to request API
  late String _jwt;

  ///TODO Use .env
  String baseRoute = 'http://aeris.com';

  AerisAPI() {
    var trigger1 = Trigger(
        service: const Service.spotify(),
        name: "Play song",
        last: DateTime.now());
    var trigger3 = Trigger(
        service: const Service.discord(),
        name: "Send a message",
        last: DateTime.now());
    var trigger2 = Trigger(
        service: const Service.spotify(),
        name: "Play song",
        last: DateTime.parse("2022-01-01"));
    var reaction = Reaction(
        service: const Service.twitter(), parameters: {}, name: "Post a tweet");
    var reaction2 = Reaction(
        service: const Service.gmail(), parameters: {}, name: "Do smth");
    var reaction1 = Reaction(
        service: const Service.youtube(),
        parameters: {},
        name: "Do smth youtube");
    var pipeline1 = Pipeline(
        id: 10,
        name: "My Action",
        triggerCount: 1,
        enabled: true,
        trigger: trigger1,
        reactions: [reaction]);
    var pipeline2 = Pipeline(
        id: 10,
        name: "My very long action Action",
        triggerCount: 10,
        enabled: true,
        trigger: trigger2,
        reactions: [reaction, reaction1, reaction2]);
    var pipeline3 = Pipeline(
        id: 10,
        name: "Disabled",
        triggerCount: 3,
        enabled: false,
        trigger: trigger3,
        reactions: [reaction]);
    fakeAPI = [
      pipeline3,
      pipeline2,
      pipeline1,
      pipeline3,
      pipeline2,
      pipeline1,
      pipeline3,
      pipeline2,
      pipeline1
    ];
  }

  /// Name of the file that contains the JWT used for Aeris' API requestd
  static const String jwtFile = 'aeris_jwt.txt';

  /// Retrieves the file containing the JWT
  Future<File> getJWTFile() async {
    final directory = await getApplicationDocumentsDirectory();
    final path = directory.path;
    return File('$path/$jwtFile.txt');
  }

  ///ROUTES
  /// Registers new user in the database and connects it. Returns false if register failed
  Future<bool> signUpUser(String username, String password) async {
    http.Response response =
        await _requestAPI('/auth/signup', AerisAPIRequestType.post, {
      username: username,
      password: password,
    });
    if (!response.ok) {
      return false;
    }
    return createConnection(username, password);
  }

  /// On success, sets API as connected to given user. Returns false if connection false
  Future<bool> createConnection(String username, String password) async {
    http.Response response =
        await _requestAPI('/auth/login', AerisAPIRequestType.post, {
      username: username,
      password: password,
    });
    if (!response.ok) {
      return false;
    }
    try {
      final String jwt = response.headers[HttpHeaders.setCookieHeader]!
          .split(';')
          .where((element) => element.trim().startsWith('JWT-Cookie='))
          .first
          .replaceAll('JWT-Cookie=', "")
          .trim();

      final File jwtFile = await getJWTFile();
      jwtFile.writeAsString(jwt);
      _connected = true;
      _jwt = jwt;
    } catch (e) {
      return false;
    }
    return true;
  }

  /// Create an API connection using previously created credentials
  Future<void> restoreConnection() async {
    try {
      final file = await getJWTFile();
      final cred = await file.readAsString();
      if (cred == "") {
        throw Exception("Empty creds");
      }
      _jwt = cred;
      _connected = true;
    } catch (e) {
      return;
    }
  }

  /// Delete JWT file and disconnect from API
  Future<void> stopConnection() async {
    File credentials = await getJWTFile();

    if (credentials.existsSync()) {
      await credentials.delete();
    }
    _connected = false;
  }

  /// Adds new pipeline to API, returns false if post failed
  Future<bool> createPipeline(Pipeline newPipeline) async {
    fakeAPI.add(newPipeline);
    var res = await _requestAPI(
        '/workflow', AerisAPIRequestType.post, newPipeline.toJSON());

    return res.ok;
  }

  /// Removes pipeline from API
  Future<bool> removePipeline(Pipeline pipeline) async {
    var res = await _requestAPI(
        '/workflow/${pipeline.id}', AerisAPIRequestType.delete, null);
    return res.ok;
  }

  /// Send PUT request to update Pipeline, returns false if failed
  Future<bool> editPipeline(Pipeline updatedPipeline) async {
    var res = await _requestAPI('/workflow/${updatedPipeline.id}',
        AerisAPIRequestType.put, updatedPipeline.toJSON());
    return res.ok;
  }

  /// Fetches the Pipelines from the API
  Future<List<Pipeline>> getPipelines() async {
    var res = await _requestAPI('/workflows', AerisAPIRequestType.get, null);
    List<Object> body = jsonDecode(res.body);

    ///TODO error handling
    ///TODO return body.map((e) => Pipeline.fromJSON(e as Map<String, Object>)).toList();
    return fakeAPI;
  }

  /// Disconnects the user from the service
  Future<bool> disconnectService(Service service) async {
    var res = await _requestAPI('/auth/${service.name.toLowerCase()}',
        AerisAPIRequestType.delete, null);
    return res.ok;
  }

  Future<List<ActionTemplate>> getActionsFor(
      Service service, Action action) async {
    await Future.delayed(const Duration(seconds: 3));
    if (action is Trigger) {
      ///TODO get triggers
    } else if (action is Reaction) {
      ///TODO get reactions
    }
    return [
      for (int i = 0; i <= 10; i++)
        ActionTemplate(
            service: service,
            name: "action$i",
            parameters: {'key1': 'value1', 'key2': 'value2'})
    ];
  }

  /// Encodes Uri for request
  Uri _encoreUri(String route) {
    return Uri.parse('$baseRoute$route');
  }

  /// Calls API using a HTTP request type, a route and body
  Future<http.Response> _requestAPI(
      String route, AerisAPIRequestType requestType, Object? body) async {
    final Map<String, String>? header =
        _connected ? {'authorization': 'Bearer $_jwt'} : null;
    switch (requestType) {
      case AerisAPIRequestType.delete:
        return await http.delete(_encoreUri(route),
            body: body, headers: header);
      case AerisAPIRequestType.get:
        return await http.get(_encoreUri(route), headers: header);
      case AerisAPIRequestType.post:
        return await http.post(_encoreUri(route), body: body, headers: header);
      case AerisAPIRequestType.put:
        return await http.put(_encoreUri(route), body: body, headers: header);
    }
  }
}
