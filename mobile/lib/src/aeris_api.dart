import 'dart:async';
import 'dart:io';
import 'package:aeris/src/models/action.dart';
import 'package:aeris/src/models/action_template.dart';
import 'package:aeris/src/models/pipeline.dart';
import 'package:aeris/src/models/reaction.dart';
import 'package:aeris/src/models/service.dart';
import 'package:aeris/src/models/trigger.dart';
import 'package:path_provider/path_provider.dart';
import 'package:http/http.dart' as http;

/// Requests types supported by Aeris API
enum AerisAPIRequestType { get, post, put, delete }

/// Call to interact with Aeris' Back end
class AerisAPI {
  ///TODO set status based on stored credentials
  bool connected = false;
  late List<Pipeline> fakeAPI;

  /// JWT token used to request API
  late String jwt;

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

  /// Registers new user in the database and connects it
  Future<void> signUpUser(String username, String password) async {}

  /// On success, sets API as connected to given user
  Future<bool> createConnection(String username, String password) async {
    http.Response response =
        await _requestAPI('/auth/login', AerisAPIRequestType.post, {
      username: username,
      password: password,
    });
    if (response.statusCode != 200) {
      return false;
    }
    final String jwt = response.headers[HttpHeaders.setCookieHeader]!
        .split(';')
        .where((element) => element.trim().startsWith('JWT-Cookie='))
        .first
        .replaceAll('JWT-Cookie=', "").trim();

    final File jwtFile = await getJWTFile();
    jwtFile.writeAsString(jwt);
    response.headers['Set-Cookie'];
    connected = true;
    this.jwt = jwt;
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
      jwt = cred;
      connected = true;
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
    connected = false;
  }

  /// Adds new pipeline to API
  Future<void> createPipeline(Pipeline newPipeline) async {
    ///TODO Send Pipeline to API
    fakeAPI.add(newPipeline);
    await Future.delayed(const Duration(seconds: 2));
    return;
  }

  /// Removes pipeline from API
  Future<void> removePipeline(Pipeline pipeline) async {
    ///TODO Send delete request to API
    fakeAPI.remove(pipeline);
    await Future.delayed(const Duration(seconds: 2));
    return;
  }

  Future<void> editPipeline(Pipeline updatedPipeline) async {
    ///TODO Send update request to API
    for (var pipeline in fakeAPI) {
      if (pipeline.id == updatedPipeline.id) {
        ///TODO Call Api
        break;
      }
    }

    await Future.delayed(const Duration(seconds: 2));
    return;
  }

  /// Fetches the Pipelines from the API
  Future<List<Pipeline>> getPipelines() async {
    /// TODO Fetch the API
    await Future.delayed(const Duration(seconds: 2));
    return fakeAPI;
  }

  /// Disconnects the user from the service
  Future<void> disconnectService(Service service) async {
    ///TODO disconnect service from user
    await Future.delayed(const Duration(seconds: 2));
    return;
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
    switch (requestType) {
      case AerisAPIRequestType.delete:
        return await http.delete(_encoreUri(route), body: body);
      case AerisAPIRequestType.get:
        return await http.get(_encoreUri(route));
      case AerisAPIRequestType.post:
        return await http.post(_encoreUri(route), body: body);
      case AerisAPIRequestType.put:
        return await http.put(_encoreUri(route), body: body);
    }
  }
}
