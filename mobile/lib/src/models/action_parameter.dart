import 'package:flutter/material.dart';

/// Object representation of an action's parameter
class ActionParameter {
  /// Name of the action parameter
  final String name;

  /// Description of theparameter
  final String description;

  /// Value of the pamrameter
  Object? value;

  ActionParameter(
      {Key? key, required this.name, this.description = "", this.value});

  static List<ActionParameter> fromJSON(Map<String, Object> params) {
    List<ActionParameter> actionParameters = [];
    params.forEach((key, value) =>
        actionParameters.add(ActionParameter(name: key, value: value)));
    return actionParameters;
  }
}
