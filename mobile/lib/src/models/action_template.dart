import 'package:aeris/src/models/action.dart';
import 'package:aeris/src/models/action_parameter.dart';
import 'package:aeris/src/models/service.dart';
import 'package:flutter/foundation.dart';

/// Template for actions, for forms
class ActionTemplate extends Action {

  ///List of values returned by the action
  final List<ActionParameter> returnedValues;

  ActionTemplate(
      {Key? key,
      required Service service,
      required String name,
      required String displayName,
      required String description,
      this.returnedValues = const [],
      List<ActionParameter> parameters = const []})
      : super(service: service, name: name, parameters: parameters, description: description, displayName: displayName);
}
