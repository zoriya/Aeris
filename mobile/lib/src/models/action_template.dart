import 'package:aeris/src/models/action.dart';
import 'package:aeris/src/models/action_parameter.dart';
import 'package:aeris/src/models/service.dart';
import 'package:flutter/foundation.dart';

/// Template for actions, for forms
class ActionTemplate extends Action {
  ActionTemplate(
      {Key? key,
      required Service service,
      required String name,
      required String description,
      List<ActionParameter> parameters = const []})
      : super(service: service, name: name, parameters: parameters, description: description);
}
