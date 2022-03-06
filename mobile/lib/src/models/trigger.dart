// ignore_for_file: hash_and_equals

import 'package:aeris/src/models/action_parameter.dart';
import 'package:aeris/src/providers/action_catalogue_provider.dart';
import 'package:flutter/material.dart';
import 'package:aeris/main.dart';
import 'package:aeris/src/models/service.dart';
import 'package:aeris/src/models/action.dart' as aeris_action;
import 'package:flutter_gen/gen_l10n/app_localizations.dart';
import 'package:provider/provider.dart';

///Object representation of a pipeline trigger
class Trigger extends aeris_action.Action {
  /// Last time the triggered was done
  final DateTime? last;
  Trigger(
      {Key? key,
      required Service service,
      required String name,
      required String displayName,
      List<ActionParameter> parameters = const [],
      this.last})
      : super(service: service, name: name, parameters: parameters, displayName: displayName);

  /// Unserialize
  static Trigger fromJSON(Object action) {
    var triggerJSON = action as Map<String, dynamic>;
    Service service =
        aeris_action.Action.parseServiceInName(triggerJSON['pType'] as String);
    var lastTriggerField = action['lastTrigger'];
    DateTime? last = lastTriggerField == null
      ? null
      : DateTime.parse(lastTriggerField as String);
    String pType = triggerJSON['pType'] as String;

    return Trigger(
        displayName: aeris_action.Action.getForCurrentLang(triggerJSON['label'])
          ?? Provider.of<ActionCatalogueProvider>(Aeris.materialKey.currentContext!, listen: false)
            .triggerTemplates[service]!.firstWhere((template) {
              return template.name == pType;
            }).displayName,
        service: service,
        name: pType,
        last: last,
        parameters: ActionParameter.fromJSON((triggerJSON['pParams'] as Map<String, dynamic>))
    );
  }

  String lastToString() {
    var context = AppLocalizations.of(Aeris.materialKey.currentContext!);
    String lastStr = context.lastTrigger;
    if (last == null) return '$lastStr: ${context.never}';
    int elapsedDays = DateTime.now().difference(last!).inDays;
    return elapsedDays == 0
        ? '$lastStr: ${context.today}'
        : '$lastStr: $elapsedDays${context.nDaysAgo}';
  }

  /// Template trigger, used as an 'empty' trigger
  Trigger.template({Key? key, this.last})
      : super(service: Service.all()[0], name: '', parameters: [], displayName: '');

  @override
  // ignore: avoid_renaming_method_parameters
  bool operator ==(Object o) {
    Trigger other = o as Trigger;
    return service.name == other.service.name &&
        name == other.name &&
        last == other.last &&
        parameters.map((e) => e.name).toString() ==
            other.parameters.map((e) => e.name).toString();
  }
}
