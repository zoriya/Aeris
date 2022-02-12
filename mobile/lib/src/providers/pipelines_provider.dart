import 'package:aeris/src/models/aeris_api.dart';
import 'package:aeris/src/models/pipeline.dart';
import 'package:flutter/cupertino.dart';
import 'package:aeris/src/models/pipeline_collection.dart';
import 'package:get_it/get_it.dart';

/// Provider class for Pipelines
class PipelineProvider extends ChangeNotifier {
  /// List of Pipelines stored in Provider
  late PipelineCollection _pipelineCollection;

  PipelineProvider() {
    _pipelineCollection = PipelineCollection(
        pipelines: [],
        sortingMethod: PipelineCollectionSort.last,
        sortingSplitDisabled: true);
    fetchPipelines();
  }

  /// Fetches the pipelines from API and put them in the collection
  fetchPipelines() {
    GetIt.I<AerisAPI>().getPipelines().then((pipelines) => _pipelineCollection.pipelines = pipelines);
  }

  /// Adds a pipeline in the Provider
  addPipeline(Pipeline newPipeline) {
    _pipelineCollection.pipelines.add(newPipeline);
    GetIt.I<AerisAPI>().createPipeline(newPipeline);
    sortPipelines();
    notifyListeners();
  }

  /// Sort pipelines inside the Provider
  sortPipelines() {
    _pipelineCollection.sort();
    notifyListeners();
  }

  /// Removes a specific pipeline from the Provider
  removePipeline(Pipeline pipeline) {
    _pipelineCollection.pipelines.remove(pipeline);
    GetIt.I<AerisAPI>().removePipeline(pipeline);
    notifyListeners();
  }
}
