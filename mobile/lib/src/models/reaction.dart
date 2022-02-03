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
  
  /// Template trigger, used as an 'empty' trigger
  Reaction.template(): super(service: Service.all()[0], name: '', parameters: {});

    @override
  bool operator ==(Object o) {
    Reaction other = o as Reaction;
    return service.name == other.service.name &&
        name == other.name &&
        parameters.values.toString() == other.parameters.values.toString() &&
        parameters.keys.toString() == other.parameters.keys.toString();
  }
}
