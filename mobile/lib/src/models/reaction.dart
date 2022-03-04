// ignore_for_file: hash_and_equals

import 'package:aeris/main.dart';
import 'package:aeris/src/models/action.dart' as aeris_action;
import 'package:aeris/src/models/action_parameter.dart';
import 'package:aeris/src/providers/action_catalogue_provider.dart';
import 'package:flutter/widgets.dart';
import 'package:aeris/src/models/service.dart';
import 'package:provider/provider.dart';

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
    String rType = reactionJSON['rType'] as String;
    var service = aeris_action.Action.parseServiceInName(rType);
    return Reaction(
        displayName: reactionJSON['label']?['en'] 
          ?? Provider.of<ActionCatalogueProvider>(Aeris.materialKey.currentContext!, listen: false)
            .reactionTemplates[service]!.firstWhere((template) {
              return template.name == rType;
            }).displayName, ///TODO use locale
        service: service,
        name: rType,
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
