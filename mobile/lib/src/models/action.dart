import 'dart:io';
import 'package:aeris/src/models/action_parameter.dart';
import 'package:flutter/widgets.dart';
import 'package:aeris/src/models/service.dart';

///Base class for reactions and trigger
abstract class Action {
  ///Action's service
  Service service;

  ///Identifier of the action type
  String name;
  ///Name odf the action
  String displayName;

  ///Action's parameters
  List<ActionParameter> parameters;

  /// Description of the action (used in catalogue)
  String? description;

  Action(
      {Key? key,
      required this.service,
      required this.name,
      required this.displayName,
      this.description,
      this.parameters = const []});

  static Service parseServiceInName(String rType) {
    var snake = rType.split('_');
    var service = snake.removeAt(0);
    return Service.factory(service);
  }

  static String? getForCurrentLang(Object? object) {
    if (object == null) return null;
    var map = Map<String, String>.from(object as dynamic);
    var key = map.keys.firstWhere(
        (lang) => Platform.localeName.contains(lang),
        orElse: () => map.keys.first);
    return map[key]!;
  }
}
