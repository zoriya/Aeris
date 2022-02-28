import 'package:aeris/src/models/action_parameter.dart';
import 'package:flutter/widgets.dart';
import 'package:aeris/src/models/service.dart';
import 'package:recase/recase.dart';
import 'package:tuple/tuple.dart';

///Base class for reactions and trigger
abstract class Action {
  ///Action's service
  Service service;

  ///Name fo the action
  String name;

  ///Action's parameters
  List<ActionParameter> parameters;
  Action(
      {Key? key,
      required this.service,
      required this.name,
      this.parameters = const []});

  static Tuple2<Service, String> parseServiceAndName(String rType) {
    var snake = ReCase(rType).snakeCase.split('_');
    return Tuple2(Service.factory(snake.first),
        ReCase(snake.getRange(1, snake.length - 1).join('_')).titleCase);
  }

  static String getType(Service service, String aName) {
    String serviceName = ReCase(service.name).pascalCase;
    String actionName = ReCase(aName).paramCase;
    return "$serviceName$actionName";
  }
}
