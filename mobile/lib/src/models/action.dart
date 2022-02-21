import 'package:flutter/widgets.dart';
import 'package:aeris/src/models/service.dart';

///Base class for reactions and trigger
abstract class Action {
  ///Action's service
  Service service;

  ///Name fo the action
  String name;

  ///Action's parameters
  Map<String, Object> parameters;
  Action(
      {Key? key,
      required this.service,
      required this.name,
      this.parameters = const {}});
}
