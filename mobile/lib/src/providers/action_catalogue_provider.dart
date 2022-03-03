import 'package:aeris/src/models/action_parameter.dart';
import 'package:aeris/src/models/action_template.dart';
import 'package:aeris/src/models/service.dart';
import 'package:flutter/cupertino.dart';
import 'package:aeris/src/aeris_api.dart';
import 'package:get_it/get_it.dart';

/// Provider class for Action listed in /about.json
class ActionCatalogueProvider extends ChangeNotifier {
  /// Tells if the provers has loaded data at least once
  final Map<Service, List<ActionTemplate>> _triggerTemplates = {};
  final Map<Service, List<ActionTemplate>> _reactionTemplates = {};
  Map<Service, List<ActionTemplate>> get triggerTemplates => _triggerTemplates;
  Map<Service, List<ActionTemplate>> get reactionTemplates =>
      _reactionTemplates;

  void reloadCatalogue() {
    Service.all().forEach((element) {
      _triggerTemplates.putIfAbsent(element, () => []);
      _reactionTemplates.putIfAbsent(element, () => []);
    });
    notifyListeners();
    GetIt.I<AerisAPI>().getAbout().then((about) {
      if (about.isEmpty || about == null) return;
      final services = (about['server'] as Map<String, dynamic>)['services'] as List<dynamic>;
      for (var serviceContent in services) {
        Service service = Service.factory(serviceContent['name']);
        for (var action in (serviceContent['actions'] as List)) {
          _triggerTemplates[service]!.add(
            ActionTemplate(
              name: action['name'],
              service: service,
              description: action['description'],
              parameters: (action['params'] as List).map(
                (e) => ActionParameter(name: e['name'], description: e['description'])
              ).toList(),
              returnedValues: (action['returns'] as List).map(
                (e) => ActionParameter(name: e['name'], description: e['description'])
              ).toList(),
            )
          );
        }
        for (var reaction in serviceContent['reactions']) {
          _reactionTemplates[service]!.add(
            ActionTemplate(
              name: reaction['name'],
              service: service,
              description: reaction['description'],
              parameters: (reaction['params'] as List).map(
                (e) => ActionParameter(name: e['name'], description: e['description'])
              ).toList(),
              returnedValues: (reaction['returns'] as List).map(
                (e) => ActionParameter(name: e['name'], description: e['description'])
              ).toList(),
            )
          );
        }
      }
      notifyListeners();
    });
  }

  ActionCatalogueProvider() {
    reloadCatalogue();
  }
}
