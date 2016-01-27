$( document ).ready(function() {
    $('.select-field-type').change(function (e) {
        var input = $('.select-field-type option:selected').attr('value');
        $('.temp-field').empty();
    });

    $('.template-name').keypress(function (e) {
        $('.new-template-name').text($(this).val());
    });

    $('.new-template-add-field').click(function (e) {
        e.preventDefault();
        var selected = $('.select-field-type option:selected');
        var type = selected.attr('data-type');
        var id = selected.attr('data-id');
        var defaults = selected.attr('data-defaults');
        var name = selected.text();
        $('.new-template-fields').append('<tr class="field-'+ id  +'"><td>'+ name +'</td><td>'+ type +'</td><td>'+ defaults +'</td><td><a href="#" data-id="'+ id +'" class="remove-field"><span class="glyphicon glyphicon-remove red"></a></span></td></tr>');
        $('tr.field-'+ id +'').append('<input class="hidden field-'+ id  +'" name="fields[]" value="'+ id +'">');
    });

    $('.new-template-fields').on("click", "a.remove-field" , function (e) {
        var id = $(this).data("id");
        $(this).parents("tr").remove();
    });

    $('.save-template').click(function (e) {
        fields = $(".new-template-fields-form");
        $('.new-template-fields-form').submit();
    });

    $('.new-item-template').change(function (e) {
        $('.soft-hidden').hide();
        var selected = $('.new-item-template option:selected');
        var id = selected.attr("data-id");
        $('.template-' + id).show();
    });
});
