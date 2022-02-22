// ignore_for_file: hash_and_equals

import 'package:aeris/src/models/action.dart' as aeris_action;
import 'package:flutter/widgets.dart';
import 'package:aeris/src/models/service.dart';
import 'package:tuple/tuple.dart';

///Object representation of a reaction
class Reaction extends aeris_action.Action {
  Reaction(
      {Key? key,
      required Service service,
      required String name,
      Map<String, Object> parameters = const {}})
      : super(service: service, name: name, parameters: parameters);

  /// Template trigger, used as an 'empty' trigger
  Reaction.template()
      : super(service: Service.all()[0], name: '', parameters: {});

  static Reaction fromJSON(Object reaction) {
    var reactionJSON = reaction as Map<String, Object>;
    Tuple2<Service, String> service =
        aeris_action.Action.parseServiceAndName(reactionJSON['rType'] as String);
    return Reaction(
        service: service.item1,
        name: service.item2,
        parameters: (reactionJSON['rParams'] as Map<String, Object>)['contents']
            as Map<String, Object>);
  }

  @override
  bool operator ==(Object other) {
    Reaction otherReaction = other as Reaction;
    return service.name == otherReaction.service.name &&
        name == otherReaction.name &&
        parameters.values.toString() ==
            otherReaction.parameters.values.toString() &&
        parameters.keys.toString() == otherReaction.parameters.keys.toString();
  }
}
