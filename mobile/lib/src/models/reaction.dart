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
  bool operator ==(Object other) {
    Reaction otherReaction = other as Reaction;
    return service.name == otherReaction.service.name &&
        name == otherReaction.name &&
        parameters.values.toString() == otherReaction.parameters.values.toString() &&
        parameters.keys.toString() == otherReaction.parameters.keys.toString();
  }
}
