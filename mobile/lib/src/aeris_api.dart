// ignore_for_file: unused_import

import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'package:aeris/main.dart';
import 'package:aeris/src/models/action.dart' as aeris;
import 'package:aeris/src/models/action_parameter.dart';
import 'package:aeris/src/models/action_template.dart';
import 'package:aeris/src/models/pipeline.dart';
import 'package:aeris/src/models/reaction.dart';
import 'package:aeris/src/models/service.dart';
import 'package:aeris/src/models/trigger.dart';
import 'package:aeris/src/providers/action_catalogue_provider.dart';
import 'package:flutter/material.dart';
import 'package:get_it/get_it.dart';
import 'package:path_provider/path_provider.dart';
import 'package:http/http.dart' as http;
import 'package:provider/provider.dart';
import 'package:shared_preferences/shared_preferences.dart';

extension IsOk on http.Response {
  bool get ok => (statusCode ~/ 100) == 2;
}

/// Requests types supported by Aeris API
enum AerisAPIRequestType { get, post, put, delete }

/// Call to interact with Aeris' Back end
class AerisAPI {
  /// Get Connection state
  bool _connected = false; //TODO Will be false later
  bool get isConnected => _connected;

  /// JWT token used to request API
  late String _jwt;

  late final String deepLinkRoute;

  String _baseRoute = GetIt.I<SharedPreferences>().getString('api') 
    ?? "http://10.0.2.2:8080";
  String get baseRoute => _baseRoute;
  set baseRoute(value) => _baseRoute = value;

  AerisAPI() {
    var scheme = "http";
    if (Theme.of(Aeris.materialKey.currentContext!).platform ==
        TargetPlatform.iOS) {
      scheme = "aeris";
    }
    deepLinkRoute = "$scheme://aeris.com";
  }

  /// Name of the file that contains the JWT used for Aeris' API requestd
  static const String jwtFile = 'aeris_jwt.txt';

  ///ROUTES
  /// Registers new user in the database and connects it. Returns false if register failed
  Future<bool> signUpUser(String username, String password) async {
    http.Response response =
        await _requestAPI('/auth/signup', AerisAPIRequestType.post, {
      'username': username,
      'password': password,
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
      'username': username,
      'password': password,
    });
    if (!response.ok) {
      return false;
    }
    try {
      final String jwt = jsonDecode(response.body)['jwt'];
      await GetIt.I<SharedPreferences>().setString('jwt', jwt);
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
      final cred = GetIt.I<SharedPreferences>().getString('jwt');
      if (cred == "" || cred == null) {
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
    await GetIt.I<SharedPreferences>().remove('jwt');
    _connected = false;
  }

  ///Get /about.json
  Future<Map<String, dynamic>> getAbout() async {
    var res = await _requestAPI('/about.json', AerisAPIRequestType.get, null);
    if (!res.ok) return {};
    return jsonDecode(res.body);
  }

  /// Adds new pipeline to API, returns false if post failed
  Future<bool> createPipeline(Pipeline newPipeline) async {
    var res = await _requestAPI(
        '/workflow', AerisAPIRequestType.post, newPipeline.toJSON());
    newPipeline.id = Pipeline.fromJSON(jsonDecode(res.body)).id;
    return res.ok;
  }

  /// Removes pipeline from API
  Future<bool> removePipeline(Pipeline pipeline) async {
    var res = await _requestAPI(
        '/workflow/${pipeline.id}', AerisAPIRequestType.delete, null);
    return res.ok;
  }

  String getServiceAuthURL(Service service) {
    final serviceName = service == const Service.youtube()
        ? "google"
        : service.name.toLowerCase();
    return "$baseRoute/auth/$serviceName/url?redirect_uri=$deepLinkRoute/authorization/$serviceName";
  }

  /// Send PUT request to update Pipeline, returns false if failed
  Future<bool> editPipeline(Pipeline updatedPipeline) async {
    var res = await _requestAPI('/workflow/${updatedPipeline.id}',
        AerisAPIRequestType.put, updatedPipeline.toJSON());
    print(res.body);
    print(res.statusCode);
    return res.ok;
  }

  /// Fetches the Pipelines from the API
  Future<List<Pipeline>> getPipelines() async {
    var res = await _requestAPI('/workflows', AerisAPIRequestType.get, null);
    if (res.ok == false) return [];
    print(res.body);
    final List body = jsonDecode(res.body);

    return body.map((e) => Pipeline.fromJSON(Map.from(e))).toList();
  }

  /// Fetch the services the user is authenticated to
  Future<List<Service>> getConnectedService() async {
    var res =
        await _requestAPI('/auth/services', AerisAPIRequestType.get, null);
    if (!res.ok) return [];
    return (jsonDecode(res.body) as List)
        .map((e) => Service.factory(e.toString())).toList();
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
        '/auth/${service.name.toLowerCase()}?code=$code&redirect_uri=$deepLinkRoute/authorization/${service.name.toLowerCase()}',
        AerisAPIRequestType.get,
        null);
    return res.ok;
  }

  List<ActionTemplate> getActionsFor(Service service, aeris.Action action) {
    final catalogue = Aeris.materialKey.currentContext?.read<ActionCatalogueProvider>();
    if (action is Trigger) {
      return catalogue!.triggerTemplates[service]!;
    }
    return catalogue!.reactionTemplates[service]!;
  }

  /// Encodes Uri for request
  Uri _encoreUri(String route) {
    return Uri.parse('$_baseRoute$route');
  }

  /// Calls API using a HTTP request type, a route and body
  Future<http.Response> _requestAPI(
      String route, AerisAPIRequestType requestType, Object? body) async {
    final Map<String, String> header = {
      'Content-type' : 'application/json', 
      'Accept': 'application/json',
    };
    if (_connected) {
      header.addAll({'Authorization': 'Bearer $_jwt'});
    }
    const duration = Duration(seconds: 3);
    try {
      switch (requestType) {
        case AerisAPIRequestType.delete:
          return await http.delete(_encoreUri(route),
              body: jsonEncode(body), headers: header).timeout(duration,
            onTimeout: () {
              return http.Response('Error', 408);
            },);
        case AerisAPIRequestType.get:
          return await http.get(_encoreUri(route), headers: header).timeout(
            duration,
            onTimeout: () {
              return http.Response('Error', 408);
            },);
        case AerisAPIRequestType.post:
          return await http.post(_encoreUri(route), body: jsonEncode(body), headers: header).timeout(duration,
            onTimeout: () {
              return http.Response('Error', 408);
            },);
        case AerisAPIRequestType.put:
          return await http.put(_encoreUri(route), body: jsonEncode(body), headers: header).timeout(duration,
            onTimeout: () {
              return http.Response('Error', 408);
            },);
      }
    } catch (e) {
      return http.Response('{}', 400);
    }
  }
}
