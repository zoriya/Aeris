import 'package:aeris/src/models/action_parameter.dart';
import 'package:flutter/widgets.dart';
import 'package:aeris/src/models/service.dart';
import 'package:recase/recase.dart';

///Base class for reactions and trigger
abstract class Action {
  ///Action's service
  Service service;

  ///Name fo the action
  String name;

  ///Action's parameters
  List<ActionParameter> parameters;

  /// Description of the action (used in catalogue)
  String? description;

  Action(
      {Key? key,
      required this.service,
      required this.name,
      this.description,
      this.parameters = const []});

  static Service parseServiceInName(String rType) {
    var snake = rType.split('_');
    var service = snake.removeAt(0);
    return Service.factory(service);
  }

  String displayName() {
    var words = name.split('_');
    words.removeAt(0);
    return ReCase(words.join()).titleCase;
  }
}
