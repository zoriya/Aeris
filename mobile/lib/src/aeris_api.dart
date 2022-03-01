// ignore_for_file: unused_import

import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'package:aeris/src/main.dart';
import 'package:aeris/src/models/action.dart';
import 'package:aeris/src/models/action_parameter.dart';
import 'package:aeris/src/models/action_template.dart';
import 'package:aeris/src/models/pipeline.dart';
import 'package:aeris/src/models/reaction.dart';
import 'package:aeris/src/models/service.dart';
import 'package:aeris/src/models/trigger.dart';
import 'package:aeris/src/providers/action_catalogue_provider.dart';
import 'package:path_provider/path_provider.dart';
import 'package:http/http.dart' as http;
import 'package:flutter_dotenv/flutter_dotenv.dart';
import 'package:provider/provider.dart';

extension IsOk on http.Response {
  bool get ok => (statusCode ~/ 100) == 2;
}

/// Requests types supported by Aeris API
enum AerisAPIRequestType { get, post, put, delete }

/// Call to interact with Aeris' Back end
class AerisAPI {
  /// Get Connection state
  bool _connected = true; //TODO Will be false later
  bool get isConnected => _connected;

  late List<Pipeline> fakeAPI;

  /// JWT token used to request API
  late String _jwt;

  late final String baseRoute;

  AerisAPI() {
    baseRoute = dotenv.env['HOSTNAME']!;
    var trigger1 = Trigger(
        service: const Service.spotify(), name: "Play song", last: DateTime.now());
    var trigger3 = Trigger(
        service: const Service.discord(),
        name: "Send a message",
        last: DateTime.now());
    var trigger2 = Trigger(
        service: const Service.spotify(),
        name: "Play song",
        last: DateTime.parse("2022-01-01"));
    var reaction = Reaction(
        service: const Service.twitter(), parameters: [], name: "Post a tweet");
    var reaction2 =
        Reaction(service: const Service.anilist(), parameters: [], name: "Do smth");
    var reaction1 = Reaction(
        service: const Service.youtube(), parameters: [], name: "Do smth youtube");
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
      final String jwt = jsonDecode(response.body)['jwt'];
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

  ///Get /about.json
  Future<Map<String, dynamic>> getAbout() async {
    var res = await _requestAPI('/about.json', AerisAPIRequestType.get, null);
    return jsonDecode(res.body);
  }

  /// Adds new pipeline to API, returns false if post failed
  Future<bool> createPipeline(Pipeline newPipeline) async {
    fakeAPI.add(newPipeline);
    var res = await _requestAPI(
        '/workflow', AerisAPIRequestType.post, newPipeline.toJSON());
    newPipeline = Pipeline.fromJSON(jsonDecode(res.body));
    return res.ok;
  }

  /// Removes pipeline from API
  Future<bool> removePipeline(Pipeline pipeline) async {
    var res = await _requestAPI(
        '/workflow/${pipeline.id}', AerisAPIRequestType.delete, null);
    return res.ok;
  }

  String getServiceAuthURL(Service service) {
    final serviceName = service.name.toLowerCase();
    return "$baseRoute/auth/$serviceName/url?redirect_uri=aeris://aeris.com/authorization/$serviceName";
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
    if (res.ok == false) return [];
    List<Object> body = jsonDecode(res.body);

    ///TODO return body.map((e) => Pipeline.fromJSON(e as Map<String, Object>)).toList();
    return fakeAPI;
  }

  /// Fetch the services the user is authenticated to
  Future<List<Service>> getConnectedService() async {
    var res =
        await _requestAPI('/auth/services', AerisAPIRequestType.get, null);
    if (!res.ok) return [];
    return (jsonDecode(res.body) as List<String>)
        .map((e) => Service.factory(e)).toList();
  }

  /// Disconnects the user from the service
  Future<bool> disconnectService(Service service) async {
    var res = await _requestAPI('/auth/${service.name.toLowerCase()}',
        AerisAPIRequestType.delete, null);
    return res.ok;
  }

  /// Connects the user from the service
  Future<bool> connectService(Service service, String code) async {
    var res = await _requestAPI(
        '/auth/${service.name.toLowerCase()}?code=$code',
        AerisAPIRequestType.get,
        null);
    return res.ok;
  }

  Future<List<ActionTemplate>> getActionsFor(
      Service service, Action action) async {
    final catalogue = Aeris.materialKey.currentContext?.read<ActionCatalogueProvider>();
    if (action is Trigger) {
      return catalogue!.triggerTemplates[service]!;
    }
    return catalogue!.reactionTemplates[service]!;
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
