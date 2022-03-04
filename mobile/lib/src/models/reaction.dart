// ignore_for_file: hash_and_equals

import 'package:aeris/src/models/action.dart' as aeris_action;
import 'package:aeris/src/models/action_parameter.dart';
import 'package:flutter/widgets.dart';
import 'package:aeris/src/models/service.dart';

///Object representation of a reaction
class Reaction extends aeris_action.Action {
  Reaction(
      {Key? key,
      required Service service,
      required String name,
      required String displayName,
      List<ActionParameter> parameters = const []})
      : super(service: service, name: name, parameters: parameters, displayName: displayName);

  /// Template trigger, used as an 'empty' trigger
  Reaction.template()
      : super(service: Service.all()[0], name: '', parameters: [],displayName: '');

  static Reaction fromJSON(Object reaction) {
    var reactionJSON = reaction as Map<String, dynamic>;
    var service = aeris_action.Action.parseServiceInName(reactionJSON['rType'] as String);
    return Reaction(
        displayName: reactionJSON['label']['en'], ///TODO use locale
        service: service,
        name: reactionJSON['rType'] as String,
        parameters: ActionParameter.fromJSON((reactionJSON['rParams'] as Map<String, dynamic>)));
  }

  /// Serialize Reaction to JSON
  Object toJSON() => {
    "rType": name,
    "rParams": { for (var e in parameters) e.name : e.value }
  };

  @override
  bool operator ==(Object other) {
    Reaction otherReaction = other as Reaction;
    return service.name == otherReaction.service.name &&
        name == otherReaction.name &&
        parameters.map((e) => e.name).toString() == other.parameters.map((e) => e.name).toString();
  }
}
