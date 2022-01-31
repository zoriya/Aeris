import 'package:mobile/src/models/action.dart' as aeris_action;
import 'package:flutter/widgets.dart';
import 'package:mobile/src/models/service.dart';

///Object representation of a reaction
class Reaction extends aeris_action.Action {
  Reaction(
      {Key? key,
      required Service service,
      required String name,
      Map<String, Object?> parameters = const {}})
      : super(service: service, name: name, parameters: parameters);
}
