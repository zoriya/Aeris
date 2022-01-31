import 'package:mobile/src/models/action.dart' as aeris_action;
import 'package:flutter/widgets.dart';
import 'package:mobile/src/models/service.dart';

// Object representation of a reaction
class Reaction extends aeris_action.Action {
  const Reaction({Key? key, required Service service, required String name, Object parameters = const {}})
      : super(service: service, name: name, parameters: parameters);
}
