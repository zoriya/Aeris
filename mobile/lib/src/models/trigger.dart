import 'package:flutter/material.dart';
import 'package:mobile/src/models/service.dart';
import 'package:mobile/src/models/action.dart' as aeris_action;

///Object representation of a pipeline trigger
class Trigger extends aeris_action.Action {
  /// Last time the triggered was done
  final DateTime? last;
  Trigger(
      {Key? key,
      required Service service,
      required String name,
      Map<String, Object?> parameters = const {},
      this.last})
      : super(service: service, name: name, parameters: parameters);

  ///TODO Constructor from DB 'Type' field
  String lastToString() {
    if (last == null) return 'Last: Never';
    int elapsedDays = DateTime.now().difference(last!).inDays;
    return elapsedDays == 0
        ? 'Last: Today'
        : 'Last: ${elapsedDays.toString()}d ago';
  }

  /// Template trigger, used as an 'empty' trigger
  Trigger.template({Key? key, this.last})
      : super(service: const Service.twitter(), name: '', parameters: {});

  @override
  bool operator ==(Object o) {
    Trigger other = o as Trigger;
    return service.name == other.service.name &&
        name == other.name &&
        last == other.last &&
        parameters.values.toString() == other.parameters.values.toString() &&
        parameters.keys.toString() == other.parameters.keys.toString();
  }
}
