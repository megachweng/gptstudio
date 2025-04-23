$(document).on('click', '.btn-clipboard', function(event) {

  // get the parent div of the parent div and next pre tag
  const parentDiv = $(this).closest('div').parent();
  const preTag = parentDiv.next('pre');

  // find code inside pre tag
  const codeTag = preTag.find('code');
  const code = codeTag.text();

  // create temp textarea and copy the code inside it
  const tempInput = $('<textarea>').val(code);
  $('body').append(tempInput);
  tempInput.select();
  document.execCommand('copy');
  tempInput.remove();

  // update clipboard button text
  const codeButton = $(this);
  const originalContent = codeButton.html();
  codeButton.html('Copied!');

  // reset clipboard button text after 1 second
  setTimeout(function() {
    codeButton.html(originalContent);
  }, 1000);
});


// Handler for insert code button
$(document).on('click', '.btn-insert-code', function(event) {
  // get the parent div of the parent div and next pre tag
  const parentDiv = $(this).closest('div').parent();
  const preTag = parentDiv.next('pre');

  // find code inside pre tag
  const codeTag = preTag.find('code');
  const code = codeTag.text();

  // Use Shiny to send the code to R
  if (typeof Shiny !== 'undefined') {
    Shiny.setInputValue('insert_code_to_rstudio', {
      code: code,
      time: new Date().getTime() // To ensure the value is always "changed"
    });
    
    // update insert button text
    const insertButton = $(this);
    const originalContent = insertButton.html();
    insertButton.html('Inserted!');

    // reset insert button text after 1 second
    setTimeout(function() {
      insertButton.html(originalContent);
    }, 1000);
  }
});


// gpt-created
function addCopyBtn() {
  // Get all the pre tags in the document that don't already have a copy button
  var preTags = $('pre:not(".hasCopyButton")');

  // Loop through all the pre tags
  preTags.each(function() {
    // Add class to indicate that the copy button has been added
    $(this).addClass('hasCopyButton');

    // Get the code element inside pre tag and its language class
    const codeTag = $(this).find('code');
    var language = codeTag.attr('class');
    if (language == undefined) {
      language = 'output';
    }

    // Create a div element with the copy button and language text
    // The svg icon was generated using Bootrap library via R
    var div = $(`
    <div class="d-flex justify-content-between bg-dark" style="border-radius: 5px 5px 0 0">
    <p class="px-2 py-1 m-0 text-muted small">${language}</p>
    <div>
        <button type="button" class="btn action-button btn-secondary btn-sm btn-insert-code shiny-bound-input mr-1">
            <i class="bi bi-file-earmark-arrow-down"></i> Insert
        </button>
        <button type="button" class="btn action-button btn-secondary btn-sm btn-clipboard shiny-bound-input ml-auto">
            <i class="bi bi-copy"></i> Copy
        </button>
    </div>
    </div>
    `);

    // Insert the div with the copy button and language text before the pre tag
    $(this).before(div);
  });
}


$(document).on('shiny:inputchanged', function(event) {
  addCopyBtn();
});
